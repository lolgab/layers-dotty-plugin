package layers.plugin

import scala.collection.mutable

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.Type

/** Methods of `Option`, `Either` and `Try` that choose between outcomes, which is what `match` and `if` do. */
private[plugin] object Combinators:
  import tpd.*

  private val wrappers = Set(
    "scala.Option", "scala.util.Either", "scala.util.Try", "scala.util.Either.LeftProjection", "scala.util.Either.RightProjection"
  )
  // Predicates (`exists`, `forall`, `filter`) are not here: `opt.filter(_.nonEmpty)` normalises a value, it does not
  // choose between two courses of action, and any real condition inside them is counted where it branches.
  private val choosing = Set("fold", "filterOrElse", "recover", "recoverWith")

  /** The method selected and the argument lists of a call `q.m[T](a)(b)`. */
  def parts(t: Tree): Option[(Select, List[List[Tree]])] =
    @annotation.tailrec def loop(t: Tree, acc: List[List[Tree]]): (Tree, List[List[Tree]]) = t match
      case Apply(f, args)  => loop(f, args :: acc)
      case TypeApply(f, _) => loop(f, acc)
      case other           => (other, acc)
    loop(t, Nil) match
      case (s: Select, argss) => Some((s, argss))
      case _                  => None

  def lambda(t: Tree): Boolean = t match
    case Block(_, _: Closure) => true
    case Typed(e, _)          => lambda(e)
    case _                    => false

  /** A reference, or a call whose arguments are references, constants or such calls: nothing in it branches. */
  def plainCall(t: Tree)(using Context): Boolean = t match
    case _: Literal | _: Ident | _: This | _: New => true
    case Select(q, _)                              => plainCall(q)
    case Typed(e, _)                               => plainCall(e)
    case TypeApply(f, _)                           => plainCall(f)
    case Apply(f, args)                            => plainCall(f) && args.forall(plainCall)
    case _                                         => false

  /** A value that costs nothing to name: a constant, a reference, an empty collection, a constructor of such things. */
  def simple(t: Tree)(using Context): Boolean = t match
    case _: Literal | _: Ident | _: This | _: New => true
    case Select(q, _)                              => simple(q)
    case Typed(e, _)                               => simple(e)
    case Block(Nil, e)                             => simple(e)
    case TypeApply(f, _)                           => simple(f)
    case Apply(f, args) =>
      val n = f.symbol.name.toString
      (f.symbol.isConstructor || Set("apply", "empty", "s", "f", "raw", "of")(n)) && simple(f) && args.forall(simple)
    case _ => false

  /** The name of the combinator when the call hides a decision, `None` for an ordinary call. */
  def decision(a: Apply)(using Context): Option[(Select, List[List[Tree]])] =
    parts(a).filter { case (sel, argss) => isDecision(sel, argss) }

  private def isDecision(sel: Select, argss: List[List[Tree]])(using Context): Boolean =
    val sym = sel.symbol
    if !sym.exists || !sym.owner.exists then false
    else
      val owner = sym.owner.fullName.toString.stripSuffix("$")
      val name = sel.name.toString
      val onWrapper = wrappers(owner) && !sym.owner.is(Flags.ModuleClass)
      val onFactory = (owner == "scala.Option" || owner == "scala.util.Either") && sym.owner.is(Flags.ModuleClass)
      name match
        case "orElse"                  => onWrapper
        case n if choosing(n)          => onWrapper
        case "getOrElse"               => onWrapper && !argss.flatten.forall(simple)
        case "when" | "unless" | "cond" => onFactory
        case _                         => false

/** Measures methods and classes from typed trees. */
object MemberMetrics:
  import tpd.*

  /** McCabe cyclomatic complexity, a Sonar-style cognitive complexity and the deepest nesting of one method body. */
  private final class Complexity(using Context) extends TreeTraverser:
    var cyclomatic = 1
    var cognitive = 0
    var maxNesting = 0
    private var nesting = 0
    private var boolOp: String = ""

    private def nested(body: => Unit): Unit =
      nesting += 1
      maxNesting = math.max(maxNesting, nesting)
      try body
      finally nesting -= 1

    private def isEmptyElse(t: Tree): Boolean = t match
      case Literal(Constant(())) => true
      case _                     => t.isEmpty

    /** An `else if` continues the chain: it costs one but adds no nesting. */
    private def branch(t: If, chained: Boolean)(using Context): Unit =
      cyclomatic += 1
      cognitive += (if chained then 1 else 1 + nesting)
      traverse(t.cond)
      nested(traverse(t.thenp))
      t.elsep match
        case e: If => branch(e, chained = true)
        case e if isEmptyElse(e) => ()
        case e =>
          cognitive += 1
          nested(traverse(e))

    /** `opt.fold(a)(f)`, `getOrElse(compute())`, `orElse`, `Option.when` and the like are `match` and `if` written as
      * method calls: they cost what the branching they hide would cost, so that rewriting one into the other is not a
      * way to lower the score. A branch that is a lambda nests itself; a by-name branch is nested here.
      */
    private def combinator(call: (Select, List[List[Tree]]), parentOp: String)(using Context): Unit =
      val (sel, argss) = call
      val name = sel.name.toString
      // a branch that is a reference or a plain call (`getOrElse(notFound("x"))`) holds no branching: it adds no level
      def branches(args: List[Tree]): Unit =
        args.foreach(x => if Combinators.lambda(x) || Combinators.plainCall(x) then traverse(x) else nested(traverse(x)))
      if name == "orElse" then
        // an alternative chain is read like `||`: one step per link, however long
        cyclomatic += 1
        if parentOp != "orElse" then cognitive += 1
        boolOp = "orElse"
        traverse(sel.qualifier)
        boolOp = ""
        argss.flatten.foreach(traverse)
      else
        cyclomatic += 1
        cognitive += 1 + nesting
        traverse(sel.qualifier)
        argss.foreach(branches)

    def traverse(tree: Tree)(using Context): Unit =
      val parentOp = boolOp
      boolOp = ""
      tree match
        case t: If => branch(t, chained = false)
        case Match(sel, cases) =>
          traverse(sel)
          // A single-case match is a destructuring, not a decision.
          if cases.size > 1 then
            cyclomatic += cases.size - 1
            cognitive += 1 + nesting
          for c <- cases do
            if !c.guard.isEmpty then
              cyclomatic += 1
              traverse(c.guard)
            nested(traverse(c.body))
        case WhileDo(cond, body) =>
          cyclomatic += 1
          cognitive += 1 + nesting
          traverse(cond)
          nested(traverse(body))
        case Try(expr, cases, fin) =>
          traverse(expr)
          for c <- cases do
            cyclomatic += 1
            cognitive += 1 + nesting
            nested(traverse(c.body))
          traverse(fin)
        case Apply(fn @ Select(lhs, name), List(rhs))
            if fn.symbol == defn.Boolean_&& || fn.symbol == defn.Boolean_|| =>
          val op = name.toString
          cyclomatic += 1
          if parentOp != op then cognitive += 1
          boolOp = op
          traverse(lhs)
          boolOp = op
          traverse(rhs)
          boolOp = ""
        case a: Apply if Combinators.decision(a).isDefined => combinator(Combinators.decision(a).get, parentOp)
        case d: DefDef => nested(traverse(d.rhs))
        // Code the compiler pasted in from an `inline` method or a macro (a derived `ReadWriter`, say) is not code
        // anyone wrote at this place: its arguments count, its expansion does not.
        case i: Inlined => i.bindings.foreach(traverse)
        case _ => traverseChildren(tree)

  private[plugin] def lineOf(unit: CompilationUnit, offset: Int): Int = unit.source.offsetToLine(offset) + 1

  /** Characters of code per logical line. About what a method formatted at 80 columns holds. */
  private[plugin] val charsPerLine = 30

  /** Characters of one string literal that count: text is data, so a paragraph weighs about as much as a long name. */
  private[plugin] val literalCap = 12

  /** Size of the source in `[from, to)` in logical lines: non-blank characters outside comments, so that reformatting
    * (line breaks, indentation, trailing commas aside) and golfing into long lines both leave it unchanged.
    */
  private[plugin] def logicalLines(unit: CompilationUnit, from: Int, to: Int): Int =
    val src = unit.source.content
    val end = math.min(to, src.length)
    var i = math.max(from, 0)
    var chars = 0
    while i < end do
      val c = src(i)
      if c == '"' && src.startsWith("\"\"\"", i) then
        val close = new String(src).indexOf("\"\"\"", i + 3)
        val stop = if close < 0 then end else math.min(close + 3, end)
        // text is data, not logic: a block of prose, SQL or JavaScript counts as a few tokens, not by its length
        var text = 0
        while i < stop do { if !src(i).isWhitespace then text += 1; i += 1 }
        chars += 6 + math.min(text, literalCap)
      else if c == '"' || c == '\'' && i + 2 < end && (src(i + 2) == '\'' || src(i + 1) == '\\') then
        // string or char literal, up to the unescaped closing quote on the same line
        chars += 2; i += 1
        var text = 0
        while i < end && src(i) != c && src(i) != '\n' do
          if src(i) == '\\' && i + 1 < end then { text += 1; i += 1 }
          text += 1
          i += 1
        if i < end && src(i) == c then i += 1
        chars += math.min(text, literalCap)
      else if c == '/' && i + 1 < end && src(i + 1) == '/' then
        while i < end && src(i) != '\n' do i += 1
      else if c == '/' && i + 1 < end && src(i + 1) == '*' then
        var depth = 1
        i += 2
        while i < end && depth > 0 do
          if src.startsWith("/*", i) then { depth += 1; i += 2 }
          else if src.startsWith("*/", i) then { depth -= 1; i += 2 }
          else i += 1
      else
        if !c.isWhitespace then chars += 1
        i += 1
    math.max(1, (chars + charsPerLine - 1) / charsPerLine)

  /** Compiler-generated `$default$N` getters are not code anyone wrote. */
  private def isDefaultGetter(sym: Symbol)(using Context): Boolean = sym.name.toString.contains("$default$")

  private[plugin] def measurable(d: DefDef)(using Context): Boolean =
    val sym = d.symbol
    sym.exists && !sym.isConstructor && !sym.isOneOf(Flags.Synthetic | Flags.Deferred) && !d.rhs.isEmpty &&
    !isDefaultGetter(sym)

  /** Members of classes and objects only: local definitions count towards their enclosing method. */
  def method(d: DefDef, unit: CompilationUnit)(using Context): Option[MethodFact] =
    val sym = d.symbol
    if !measurable(d) || !sym.owner.isClass || sym.owner.isAnonymousClass || !d.span.exists then None
    else
      val params = d.paramss.flatten.count {
        case v: ValDef => !v.symbol.isOneOf(Flags.GivenOrImplicit)
        case _         => false
      }
      Some(fact(sym.fullName.toString, sym, List(d.rhs), d.span, params, unit))

  /** A `val`, `lazy val` or `given` of a class or object is code that runs like a method body: a table of lambdas or a
    * long initialiser must not escape the size and complexity limits just because it is not written with `def`.
    */
  def valMember(v: ValDef, unit: CompilationUnit)(using Context): Option[MethodFact] =
    val sym = v.symbol
    if !sym.exists || !sym.owner.isClass || sym.owner.isAnonymousClass || !v.span.exists || v.rhs.isEmpty ||
      sym.isOneOf(Flags.Synthetic | Flags.Deferred | Flags.ParamAccessor | Flags.CaseAccessor | Flags.Module) ||
      sym.name.toString.contains("$") || sym.is(Flags.Param)
    then None
    else Some(fact(sym.fullName.toString, sym, List(v.rhs), v.span, 0, unit))

  /** The statements of a class or object body that are neither definitions nor imports run in the constructor: they are
    * code like any method, measured as `<Class>.<init>`.
    */
  def initializer(td: TypeDef, unit: CompilationUnit)(using Context): Option[MethodFact] =
    val sym = td.symbol
    td.rhs match
      case impl: Template if sym.isClass && !sym.isAnonymousClass && !sym.is(Flags.Synthetic) =>
        val stats = impl.body.filter {
          case _: DefTree | _: ImportOrExport => false
          case t                               => !t.isEmpty && t.span.exists
        }
        if stats.isEmpty then None
        else
          val span = stats.map(_.span).reduce(_.union(_))
          Some(fact(s"${sym.fullName}.<init>", sym, stats, span, 0, unit))
      case _ => None

  /** What other code can call: methods, and the `val`s of a class or object that have an initialiser to measure. */
  private def callable(used: Symbol)(using Context): Boolean =
    used.isTerm && used.owner.isClass && used.isDefinedInCurrentRun && !isDefaultGetter(used) && !used.isConstructor &&
    (used.is(Flags.Method) ||
      !used.isOneOf(Flags.Param | Flags.ParamAccessor | Flags.CaseAccessor | Flags.Module | Flags.Synthetic | Flags.Deferred) &&
      !used.name.toString.contains("$"))

  private def fact(
      name: String, sym: Symbol, rhss: List[Tree], span: dotty.tools.dotc.util.Spans.Span, params: Int, unit: CompilationUnit
  )(using Context): MethodFact =
    val c = new Complexity
    rhss.foreach(c.traverse)
    val startLine = lineOf(unit, span.start)
    val calls = mutable.LinkedHashSet[String]()
    rhss.foreach(_.foreachSubTree { t =>
      val used = t.symbol
      if used.exists && used != sym && callable(used) then calls += used.fullName.toString
    })
    val overrides = sym.is(Flags.Method) && sym.owner.isClass && sym.allOverriddenSymbols.nonEmpty
    MethodFact(
      unit.source.file.path, // replaced by the caller with the normalised path
      lineOf(unit, span.point),
      name,
      c.cyclomatic,
      c.cognitive,
      lineOf(unit, span.end) - startLine + 1,
      logicalLines(unit, span.start, span.end),
      params,
      c.maxNesting,
      sym.is(Flags.Private),
      calls.toList,
      overrides,
      SymbolNames.packageOf(sym)
    )

  /** The package prefix that marks code as the project's own: `app` for `app.model`, `com.acme` for `com.acme.shop`. */
  private[plugin] def projectRoot(pkg: String): String =
    val parts = pkg.split('.').toList
    val tld = Set("com", "org", "io", "net", "de", "ch", "dev", "me", "eu")
    val forges = Set("github", "gitlab", "bitbucket")
    parts match
      case a :: b :: c :: _ if tld(a) && forges(b) => s"$a.$b.$c"
      case a :: b :: _ if tld(a)                   => s"$a.$b"
      case a :: _                                  => a
      case Nil                                     => pkg

  /** Domain types (outermost class names) a signature works on: the project's own classes, not the standard library,
    * frameworks or type parameters. A web framework's response type says what layer a method is in, not what it is for.
    */
  private def domainTypes(tpes: List[Type], owner: Symbol)(using Context): List[String] =
    val ownTop = SymbolNames.topLevelName(owner)
    val root = projectRoot(SymbolNames.packageOf(owner))
    tpes.flatMap(SymbolNames.classesIn).filter(c => c.isClass && !c.is(Flags.Package)).flatMap { c =>
      val pkg = SymbolNames.packageOf(c)
      if pkg == "root" || SymbolNames.isStdLib(pkg) || !(pkg == root || pkg.startsWith(root + ".")) then None
      else Some(SymbolNames.topLevelName(c))
    }.filter(n => n.nonEmpty && n != ownTop).distinct.sorted

  /** What cohesion needs: per method the domain types it works on, the siblings it calls and the fields it uses. */
  def cls(td: TypeDef, unit: CompilationUnit)(using Context): Option[ClassFact] =
    val sym = td.symbol
    td.rhs match
      case impl: Template
          if sym.isClass && (!sym.is(Flags.Synthetic) || sym.name.toString.contains("$package")) && !sym.isAnonymousClass && td.span.exists =>
        val all = impl.body.collect { case d: DefDef if measurable(d) => d }
        val (exported, methods) = all.partition(_.symbol.is(Flags.Exported))
        if all.size < 2 then None
        else
          val fields = impl.body.collect { case v: ValDef if v.symbol.exists => v.symbol }.toSet
          val members = fields ++ methods.map(_.symbol)
          val facts = methods.map { m =>
            val used = mutable.LinkedHashSet[Symbol]()
            m.rhs.foreachSubTree { t =>
              val s = t.symbol
              if s.exists && s.owner == sym && members(s) && s != m.symbol then used += s
            }
            val sigTypes =
              m.paramss.flatten.collect { case v: ValDef if !v.symbol.isOneOf(Flags.GivenOrImplicit) => v.tpt.tpe } :+
                m.tpt.tpe
            ClassMethod(
              m.symbol.name.toString,
              domainTypes(sigTypes, sym),
              used.filter(_.is(Flags.Method)).map(_.name.toString).toList.distinct,
              used.filterNot(_.is(Flags.Method)).map(f => f.name.toString + (if f.is(Flags.Mutable) then "!" else "")).toList.distinct
            )
          }
          Some(ClassFact(unit.source.file.path, lineOf(unit, td.span.point), sym.fullName.toString.stripSuffix("$"), facts, exported.size))
      case _ => None
