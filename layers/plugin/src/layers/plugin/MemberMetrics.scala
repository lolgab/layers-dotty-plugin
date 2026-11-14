package layers.plugin

import scala.collection.mutable

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.*

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
        case d: DefDef => nested(traverse(d.rhs))
        case _ => traverseChildren(tree)

  private[plugin] def lineOf(unit: CompilationUnit, offset: Int): Int = unit.source.offsetToLine(offset) + 1

  private[plugin] def measurable(d: DefDef)(using Context): Boolean =
    val sym = d.symbol
    sym.exists && !sym.isConstructor && !sym.isOneOf(Flags.Synthetic | Flags.Deferred) && !d.rhs.isEmpty

  /** Members of classes and objects only: local definitions count towards their enclosing method. */
  def method(d: DefDef, unit: CompilationUnit)(using Context): Option[MethodFact] =
    val sym = d.symbol
    if !measurable(d) || !sym.owner.isClass || sym.owner.isAnonymousClass || !d.span.exists then None
    else
      val c = new Complexity
      c.traverse(d.rhs)
      val params = d.paramss.flatten.count {
        case v: ValDef => !v.symbol.isOneOf(Flags.GivenOrImplicit)
        case _         => false
      }
      val startLine = lineOf(unit, d.span.start)
      Some(
        MethodFact(
          unit.source.file.path, // replaced by the caller with the normalised path
          lineOf(unit, d.span.point),
          sym.fullName.toString,
          c.cyclomatic,
          c.cognitive,
          lineOf(unit, d.span.end) - startLine + 1,
          params,
          c.maxNesting
        )
      )

  /** LCOM4: connected components of the graph whose nodes are the class's methods and fields, with an edge for every
    * field a method reads or writes and every sibling method it calls.
    */
  def cls(td: TypeDef, unit: CompilationUnit)(using Context): Option[ClassFact] =
    val sym = td.symbol
    td.rhs match
      case impl: Template
          if sym.isClass && !sym.isOneOf(Flags.ModuleClass | Flags.Synthetic) && !sym.isAnonymousClass && td.span.exists =>
        val methods = impl.body.collect { case d: DefDef if measurable(d) => d }
        if methods.size < 2 then None
        else
          val fields = impl.body.collect { case v: ValDef if v.symbol.exists => v.symbol }.toSet
          val members = fields ++ methods.map(_.symbol)
          val parent = mutable.Map[Symbol, Symbol]()
          def find(s: Symbol): Symbol =
            val p = parent.getOrElseUpdate(s, s)
            if p == s then s
            else
              val root = find(p)
              parent(s) = root
              root
          def union(a: Symbol, b: Symbol): Unit = parent(find(a)) = find(b)

          for m <- methods do
            find(m.symbol)
            m.rhs.foreachSubTree { t =>
              val used = t.symbol
              if used.exists && used.owner == sym && members(used) then union(m.symbol, used)
            }
          val clusters = methods.groupBy(m => find(m.symbol)).values.toList
            .map(_.map(_.symbol.name.toString).distinct)
            .sortBy(c => (-c.size, c.head))
          Some(ClassFact(unit.source.file.path, lineOf(unit, td.span.point), sym.fullName.toString, methods.size, clusters))
      case _ => None
