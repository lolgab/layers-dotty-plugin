package layers.plugin

import java.security.MessageDigest

import scala.collection.mutable

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*

import SymbolNames.{isStdLib, packageOf}

/** Extracts the raw signals behind the slop detectors from typed trees. */
object SlopMetrics:
  import tpd.*

  /** Methods smaller than this are never fingerprinted. */
  val cloneFloor = 20

  private def memberMethod(d: DefDef, unit: CompilationUnit)(using Context): Boolean =
    val sym = d.symbol
    MemberMetrics.measurable(d) && sym.owner.isClass && !sym.owner.isAnonymousClass && d.span.exists

  private def strip(t: Tree): Tree = t match
    case Block(Nil, e)       => strip(e)
    case Typed(e, _)         => strip(e)
    case Inlined(_, Nil, e)  => strip(e)
    case _                   => t

  /** The called function and the explicit arguments over all argument lists. */
  private def callOf(t: Tree, acc: List[List[Tree]] = Nil)(using Context): (Tree, List[Tree]) = t match
    case Apply(fn, args) =>
      val implicitLayer = fn.tpe.widen match
        case mt: MethodType => mt.isImplicitMethod
        case _              => false
      callOf(fn, if implicitLayer then acc else args :: acc)
    case TypeApply(fn, _) => callOf(fn, acc)
    case _                => (t, acc.flatten)

  private def argSymbol(t: Tree)(using Context): Symbol = t match
    case Typed(inner, _) => argSymbol(inner)
    case Ident(_)        => t.symbol
    case _               => NoSymbol

  /** An argument that carries no logic of its own: a literal, a reference, or `summon`. Passing one extra of these
    * does not make a wrapper do anything.
    */
  private def trivialArg(t: Tree)(using Context): Boolean = strip(t) match
    case _: Literal | _: Ident | _: This => true
    case Select(q, _)                    => trivialArg(q)
    case Apply(fn, _) if fn.symbol.name.toString == "summon" && fn.symbol.owner == defn.ScalaPredefModuleClass => true
    case TypeApply(fn, _) => trivialArg(fn)
    case _                => false

  /** A new (not overriding) method whose whole body hands every explicit parameter, each as is, to a method of another
    * class, with at most one extra constant or reference. Reordering the parameters or adding a trivial argument does
    * not make a wrapper do anything.
    */
  def passThrough(d: DefDef, unit: CompilationUnit)(using Context): Option[PassThroughFact] =
    val sym = d.symbol
    if !memberMethod(d, unit) || sym.isOneOf(Flags.Override | Flags.Inline | Flags.Given | Flags.Exported) ||
      sym.allOverriddenSymbols.nonEmpty
    then None
    else
      val params = d.paramss.flatten.collect {
        case v: ValDef if !v.symbol.isOneOf(Flags.GivenOrImplicit) => v.symbol
      }
      if params.isEmpty then None
      else
        callOf(strip(d.rhs)) match
          case (fun @ Select(qual, _), args)
              if fun.symbol.exists && !fun.symbol.isConstructor && fun.symbol.owner != sym.owner &&
                !qual.isInstanceOf[This] && !qual.isInstanceOf[Super] =>
            val passed = args.map(argSymbol)
            val extras = args.zip(passed).filter { case (_, s) => !params.contains(s) }.map(_._1)
            if params.forall(p => passed.count(_ == p) == 1) && extras.size <= 1 && extras.forall(trivialArg) then
              Some(
                PassThroughFact(
                  unit.source.file.path,
                  MemberMetrics.lineOf(unit, d.span.point),
                  sym.fullName.toString,
                  fun.symbol.fullName.toString
                )
              )
            else None
          case _ => None

  /** The case class built by `new C(..)` or `C(..)`, if the call builds one. */
  def constructed(t: Apply)(using Context): Option[String] =
    val fn = t.fun.symbol
    if !fn.exists then None
    else if fn.isConstructor && fn.owner.is(Flags.CaseClass) then Some(fn.owner.fullName.toString)
    else if fn.name.toString == "apply" && fn.is(Flags.Synthetic) && fn.owner.is(Flags.ModuleClass) &&
      fn.owner.companionClass.is(Flags.CaseClass)
    then Some(fn.owner.companionClass.fullName.toString)
    else None

  /** Case classes with at least this many fields are tracked as possible parameter bundles. */
  val minTrackedFields = 3

  def bundleCandidate(sym: Symbol)(using Context): Boolean =
    sym.isClass && sym.is(Flags.CaseClass) && !sym.is(Flags.ModuleClass) &&
      sym.primaryConstructor.exists && sym.primaryConstructor.paramSymss.flatten.count(_.isTerm) >= minTrackedFields

  /** The case classes taken as explicit parameters by a method. */
  def bundleParams(d: DefDef, unit: CompilationUnit)(using Context): List[BundleUse] =
    val sym = d.symbol
    if !memberMethod(d, unit) then Nil
    else
      d.paramss.flatten.collect {
        case v: ValDef if !v.symbol.isOneOf(Flags.GivenOrImplicit) && v.tpt.tpe.widen.classSymbol.is(Flags.CaseClass) =>
          BundleUse(
            unit.source.file.path, MemberMetrics.lineOf(unit, d.span.point), "param",
            v.tpt.tpe.widen.classSymbol.fullName.toString, sym.fullName.toString
          )
      }.distinctBy(_.sym)

  /** A case class with enough fields to be a parameter bundle. */
  def bundleDecl(td: TypeDef, unit: CompilationUnit)(using Context): Option[BundleDecl] =
    val sym = td.symbol
    td.rhs match
      case impl: Template if sym.isClass && sym.is(Flags.CaseClass) && !sym.is(Flags.ModuleClass) && td.span.exists =>
        val fields = sym.primaryConstructor.paramSymss.flatten.count(_.isTerm)
        val behaviour = impl.body.exists {
          case d: DefDef => !d.symbol.isConstructor && !d.symbol.is(Flags.Synthetic)
          case v: ValDef => !v.symbol.is(Flags.ParamAccessor) && !v.symbol.is(Flags.Synthetic)
          case _         => false
        }
        val derives = sym.companionModule.exists && sym.companionModule.moduleClass.info.decls.toList.exists(_.is(Flags.Given))
        Some(BundleDecl(unit.source.file.path, MemberMetrics.lineOf(unit, td.span.point), sym.fullName.toString, fields, behaviour, derives))
      case _ => None

  private def typeToken(tpe: Type)(using Context): String = tpe.widenDealias match
    case AppliedType(tycon, args) => typeToken(tycon) + args.map(typeToken).mkString("[", ",", "]")
    case t                        => t.typeSymbol.fullName.toString

  /** Fingerprint of a method body. Local names and literal values are normalised away, everything else (control
    * flow, called methods, types) is kept, so equal fingerprints mean the same logic over the same APIs.
    */
  def cloneFact(d: DefDef, unit: CompilationUnit)(using Context): Option[CloneFact] =
    val sym = d.symbol
    if !memberMethod(d, unit) then None
    else
      val sb = new StringBuilder
      var size = 0
      var lookupTable = false
      val locals = mutable.Map[Symbol, Int]()

      def ref(s: Symbol, name: Name): String =
        if !s.exists then "?" + name
        else if s == sym then "self"
        else if s.ownersIterator.contains(sym) then "L" + locals.getOrElseUpdate(s, locals.size)
        else s.fullName.toString

      val traverser = new TreeTraverser:
        def traverse(t: Tree)(using Context): Unit = t match
          case tt: TypeTree => sb.append("T:").append(typeToken(tt.tpe))
          case _ =>
            size += 1
            sb.append(t.getClass.getSimpleName)
            t match
              case Literal(c) => sb.append(':').append(c.tag)
              case Match(_, cases) if cases.size >= 3 && cases.count(c => strip(c.body).isInstanceOf[Literal]) * 10 >= cases.size * 7 =>
                lookupTable = true
              case r: (Ident | Select) => sb.append(':').append(ref(r.symbol, r.name))
              case v: ValDef        => sb.append(':').append(ref(v.symbol, v.name))
              case dd: DefDef       => sb.append(':').append(ref(dd.symbol, dd.name))
              case _                =>
            sb.append('(')
            traverseChildren(t)
            sb.append(')')

      d.paramss.flatten.foreach(traverser.traverse)
      traverser.traverse(d.rhs)
      // Lookup tables (a match returning a different literal per case) look alike but are data, not logic.
      if size < cloneFloor || lookupTable then None
      else
        val digest = MessageDigest.getInstance("SHA-256").digest(sb.toString.getBytes("UTF-8"))
        val hash = digest.take(8).map(b => "%02x".format(b & 0xff)).mkString
        Some(
          CloneFact(unit.source.file.path, MemberMetrics.lineOf(unit, d.span.point), sym.fullName.toString, size, hash)
        )

  def typeDecl(td: TypeDef, unit: CompilationUnit)(using Context): Option[TypeDecl] =
    val sym = td.symbol
    td.rhs match
      case _: Template
          if sym.isClass && td.span.exists && !sym.is(Flags.JavaDefined) &&
            (sym.isAnonymousClass || !sym.is(Flags.Synthetic)) =>
        val kind =
          if sym.isAnonymousClass then "anon"
          else if sym.is(Flags.ModuleClass) then "object"
          else if sym.is(Flags.Trait) then "trait"
          else if sym.is(Flags.Abstract) then "abstract"
          else "class"
        val parents = sym.info.parents.map(_.classSymbol).filter { p =>
          p.exists && p != defn.ObjectClass && p != defn.AnyClass && !isStdLib(packageOf(p))
        }.map(_.fullName.toString)
        val name = sym.fullName.toString.stripSuffix("$")
        Some(
          TypeDecl(
            unit.source.file.path,
            MemberMetrics.lineOf(unit, td.span.point),
            name,
            packageOf(sym),
            kind,
            sym.is(Flags.Sealed),
            sym.typeParams.nonEmpty,
            !sym.owner.isClass && !sym.owner.is(Flags.Package),
            parents
          )
        )
      case _ => None
