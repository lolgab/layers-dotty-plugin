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

  /** A new (not overriding) method whose whole body hands its explicit parameters, unchanged and in order, to a method
    * of another class. Overrides are skipped: delegating an interface method is how decorators and adapters work.
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
                !qual.isInstanceOf[This] && !qual.isInstanceOf[Super] &&
                args.size == params.size && args.map(argSymbol) == params =>
            Some(
              PassThroughFact(
                unit.source.file.path,
                MemberMetrics.lineOf(unit, d.span.point),
                sym.fullName.toString,
                fun.symbol.fullName.toString
              )
            )
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
