package layers.plugin

import scala.collection.mutable

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.report
import dotty.tools.dotc.util.{NoSourcePosition}
import dotty.tools.dotc.util.Spans.Span

import SymbolNames.{classesIn, isStdLib, packageOf, topLevelName}

/** What the standard library can do to a pure function: read the clock, draw random numbers, touch files, the
  * environment, the console or threads. Names are the owner class and the member, as the compiler spells them.
  */
object Ambient:
  private val nowOwners = Set(
    "java.time.LocalDate", "java.time.LocalDateTime", "java.time.LocalTime", "java.time.Instant", "java.time.YearMonth",
    "java.time.Year", "java.time.ZonedDateTime", "java.time.OffsetDateTime", "java.time.OffsetTime", "java.time.MonthDay"
  )
  private val randomOwners = Set("java.util.Random", "scala.util.Random", "java.security.SecureRandom", "java.util.concurrent.ThreadLocalRandom")
  private val fileOwners = Set(
    "java.io.FileInputStream", "java.io.FileOutputStream", "java.io.FileReader", "java.io.FileWriter", "java.io.RandomAccessFile",
    "java.nio.file.Files", "java.nio.channels.FileChannel", "scala.io.Source", "scala.io.StdIn"
  )
  private val fileMembers = Set(
    "exists", "delete", "deleteOnExit", "listFiles", "list", "mkdir", "mkdirs", "createNewFile", "createTempFile", "length", "lastModified",
    "isFile", "isDirectory", "canRead", "canWrite", "renameTo"
  )
  private val osMembers = Set(
    "read", "write", "list", "walk", "exists", "isFile", "isDir", "isLink", "remove", "move", "copy", "makeDir", "temp", "stat", "proc",
    "call", "spawn", "truncate", "perms", "owner", "mtime", "size", "readLink", "symlink", "hardlink", "watch", "pwd", "home", "stat"
  )
  private val loggingLibs = List(
    "scribe", "org.slf4j", "java.util.logging", "org.apache.logging", "com.typesafe.scalalogging", "org.apache.commons.logging",
    "ch.qos.logback", "wvlet.log"
  )

  private def systemCategory(name: String): Option[String] = name match
    case "currentTimeMillis" | "nanoTime" => Some("clock")
    case "getenv" | "getProperty" | "getProperties" | "setProperty" | "setProperties" | "clearProperty" => Some("env")
    case "out" | "err" | "in" => Some("console")
    case _ => None

  private def inLib(pkg: String, libs: List[String]) = libs.exists(p => pkg == p || pkg.startsWith(p + "."))

  /** Category and a short label, when `owner.name` is ambient: `clock`, `random`, `filesystem`, `env`, `logging`,
    * `console` or `thread`. `name` is `<init>` for constructors and `<type>` when the type itself is mentioned.
    */
  def classify(owner: String, name: String, pkg: String): Option[(String, String)] =
    val simple = owner.split('.').last
    val label = if name == "<init>" || name == "<type>" then simple else s"$simple.$name"
    val member = name != "<type>"
    val found: Option[String] =
      if nowOwners(owner) && name == "now" then Some("clock")
      else if owner == "java.time.Clock" then Some("clock")
      else if owner == "java.util.Calendar" && name == "getInstance" then Some("clock")
      else if owner == "java.net.URL" && (name == "openStream" || name == "openConnection" || name == "getContent") then Some("filesystem")
      else if owner.startsWith("java.net.http") || Set("java.net.HttpURLConnection", "java.net.Socket", "java.net.ServerSocket", "java.net.URLConnection", "java.net.DatagramSocket").contains(owner) then Some("integration")
      else if owner == "scala.concurrent.duration.Deadline" && name == "now" then Some("clock")
      else if owner == "java.lang.System" then systemCategory(name)
      else if randomOwners(owner) then Some("random")
      else if owner == "java.util.UUID" && name == "randomUUID" then Some("random")
      else if (owner == "scala.math.package" || owner == "java.lang.Math" || owner == "scala.math") && name == "random" then Some("random")
      else if fileOwners(owner) then Some("filesystem")
      else if owner == "java.io.File" && member && fileMembers(name) then Some("filesystem")
      else if owner == "scala.sys.package" && (name == "env" || name == "props") then Some("env")
      else if owner.startsWith("scala.sys.process") || owner == "java.lang.Runtime" then Some("env")
      else if owner == "scala.Predef" && Set("println", "print", "printf", "readLine").contains(name) then Some("console")
      else if owner == "scala.Console" then Some("console")
      else if owner == "java.lang.Thread" && (name == "sleep" || name == "<init>" || name == "start" || name == "<type>") then Some("thread")
      else if owner == "java.util.concurrent.Executors" || owner == "scala.concurrent.Await" then Some("thread")
      else if pkg == "os" && owner.startsWith("os") && member && osMembers(name) then Some("filesystem")
      else if inLib(pkg, loggingLibs) then Some("logging")
      else None
    found.map(c => c -> label)

/** Extracts the architecture facts from typed trees. Record kinds and their fields (after the tag, kind and line):
  *
  *  - `impure` `[member, category, label, pkg, topLevel]`: a use of the clock, randomness, IO, logging, or of persistence, web,
  *    rendering or integration code, by `member`
  *  - `ref` `[member, fromPkg, toPkg, kind, names, calls]`: which packages (and of what kind) `member` calls into
  *  - `identparam` `[member, name, type, using, compared]` and `identfield` `[class, name, type]`: parameters and
  *    fields whose type could stand for the acting identity or request context
  *  - `classinfo` `[class, pkg, kind, fieldCount, fields]`: classes with many fields
  *  - `fielduse` `[member, fromPkg, class, field, mode]`: reads (`r`) and copy updates (`w`) of those fields, and calls
  *    (`m`) of their methods
  *  - `copy` `[member, topLevel, class, field, value]`: `x.copy(field = <constant>)`
  *  - `collfield` `[class, field, elementType, derivesCodec]` and `colluse` `[class, field, op, member]`: collections of the
  *    project's own types held by a class and what is done with them
  *  - `cfg` `[key]`: the options the facts of this file were extracted with
  */
object ArchExtract:
  import tpd.*

  /** Classes with fewer fields are not tracked for the god-type check; the limit option cannot go below it. */
  val fieldFloor = 8

  private val countingOps = Set("size", "length", "isEmpty", "nonEmpty", "contains", "exists", "knownSize", "sizeIs", "lengthIs", "count", "forall")

  def extract(unit: CompilationUnit, source: String, cfg: ArchConfig)(using Context): List[ArchFact] =
    try
      val c = new Collector(unit, source, cfg)
      c.traverse(unit.tpdTree)
      c.result()
    catch
      // a symbol the extraction does not understand must not break the build: say so, and judge the rest
      case e: Exception =>
        report.warning(s"[layers] architecture checks skipped $source: ${e.getClass.getSimpleName} ${e.getMessage}", NoSourcePosition)
        Nil

  private final class Collector(unit: CompilationUnit, source: String, cfg: ArchConfig)(using Context) extends TreeTraverser:
    private val out = mutable.ListBuffer[ArchFact]()
    private val seen = mutable.Set[String]()
    private val refs = mutable.LinkedHashMap[(String, String), (String, Int, String, mutable.LinkedHashSet[String], Int)]()
    private var member: Symbol = NoSymbol
    private var memberName = ""
    private var memberPkg = ""
    private var root = ""

    def result(): List[ArchFact] =
      // each member that touches a package is also in its own package, which is one of the layers it belongs to
      for ((m, toPkg), (kind, line, fromPkg, names, n)) <- refs do
        out += ArchFact("ref", source, line, List(m, fromPkg, toPkg, kind, ArchFact.join(names.take(8)), n.toString))
      out += ArchFact("cfg", source, 0, List(cfg.extractionKey))
      out.toList

    private def line(span: Span, fallback: Symbol): Int =
      val at = if span.exists then span else if fallback.exists then fallback.span else Span(0)
      if at.exists then unit.source.offsetToLine(at.point) + 1 else 0

    private def add(kind: String, key: String, span: Span, fields: List[String]): Unit =
      if seen.add(kind + "\u0000" + key) then out += ArchFact(kind, source, line(span, member), fields)

    // ---- symbols ----------------------------------------------------------------------------------------------

    private final case class Info(pkg: String, ambient: Option[(String, String)], kind: Option[String], internal: Boolean, label: String, module: Boolean)
    private val infoCache = mutable.Map[Symbol, Info]()

    private def isProject(pkg: String) = root.nonEmpty && (pkg == root || pkg.startsWith(root + "."))

    private def info(sym: Symbol): Info =
      infoCache.getOrElseUpdate(sym, {
        val typeLike = sym.isClass || sym.is(Flags.Module)
        val cls = if sym.isClass then sym else if sym.is(Flags.Module) then sym.moduleClass else sym.owner
        val pkg = packageOf(sym)
        val ownerName = cls.fullName.toString.stripSuffix("$")
        val memberName = if typeLike then "<type>" else sym.name.toString
        val ambient = if pkg == "root" then None else Ambient.classify(ownerName, memberName, pkg)
        val internal = isProject(pkg)
        val kind = if pkg == "root" then None else PackageKinds.kind(pkg, cfg, internal)
        val simple = cls.name.toString.stripSuffix("$")
        val label = if typeLike || sym.isConstructor then simple else s"$simple.${sym.name}"
        Info(pkg, ambient, kind, internal, label, sym.is(Flags.Module))
      })

    /** Code in a package that is infrastructure by its own name cannot be domain code: what it uses is not recorded. */
    private def infraMember: Boolean =
      PackageKinds.kind(memberPkg, cfg, internal = true).exists(k => PackageKinds.infrastructure(k) || k == "auth")

    private def usable(sym: Symbol): Boolean =
      sym.exists && !sym.is(Flags.Package) && !sym.isPackageObject && !sym.is(Flags.Param) && !sym.isLocalDummy

    /** Records what `member` does with `sym`: a clock or IO call, a use of an infrastructure package, a call into a
      * layer. Only calls count as touching a layer: a type in a signature or a data accessor says nothing about what
      * a method does.
      */
    private def reference(sym: Symbol, span: Span, term: Boolean): Unit =
      if member.exists && usable(sym) && !sym.owner.is(Flags.Method) then
        val i = info(sym)
        // an object is judged by the members used from it, so `Random.nextInt` is one finding, not two
        if !i.module then
          i.ambient match
            case Some((category, label)) => add("impure", s"$memberName|$category|$label", span, List(memberName, category, label, memberPkg, topOf(member)))
            case None =>
              i.kind match
                case Some(k) if PackageKinds.infrastructure(k) && i.pkg != memberPkg && !infraMember =>
                  add("impure", s"$memberName|$k|${i.label}", span, List(memberName, k, i.label, memberPkg, topOf(member)))
                case _ => ()
        val behaviour = term && sym.is(Flags.Method) && !sym.isConstructor && !sym.isOneOf(Flags.Accessor | Flags.Synthetic | Flags.ParamAccessor | Flags.CaseAccessor)
        if behaviour && i.pkg != "root" && !isStdLib(i.pkg) && i.pkg != memberPkg then
          val kind = i.kind.orElse(if i.internal then Some("project") else None)
          // links built with the project's own route helpers are not "parsing the request"
          kind.filterNot(k => k == "web" && i.internal).foreach { k =>
            val key = (memberName, i.pkg)
            val old = refs.get(key)
            val names = old.map(_._4).getOrElse(mutable.LinkedHashSet[String]())
            names += i.label
            refs(key) = (k, old.map(_._2).getOrElse(line(span, member)), memberPkg, names, old.map(_._5).getOrElse(0) + 1)
          }

    // ---- traversal --------------------------------------------------------------------------------------------

    private def topOf(sym: Symbol): String = topLevelName(sym)

    private def isDefaultGetter(sym: Symbol) = sym.name.toString.contains("$default$")

    private def displayName(sym: Symbol): String =
      val full = sym.fullName.toString
      if isDefaultGetter(sym) then full.replaceAll("\\$default\\$\\d+", "") + " (default argument)" else full

    private def withMember(sym: Symbol)(body: => Unit): Unit =
      val (m, n, p) = (member, memberName, memberPkg)
      member = sym
      memberName = displayName(sym)
      memberPkg = packageOf(sym)
      if root.isEmpty then root = MemberMetrics.projectRoot(memberPkg)
      try body
      finally
        member = m
        memberName = n
        memberPkg = p

    def traverse(tree: Tree)(using Context): Unit = tree match
      case _: Import => ()
      case pd: PackageDef => pd.stats.foreach(traverse)
      case td: TypeDef if td.rhs.isInstanceOf[Template] && td.symbol.isClass =>
        val sym = td.symbol
        if sym.isAnonymousClass || (sym.is(Flags.Synthetic) && !sym.name.toString.contains("$package")) then traverseChildren(td)
        else
          withMember(sym) {
            if root.isEmpty then root = MemberMetrics.projectRoot(memberPkg)
            classDecl(td)
            traverseChildren(td)
          }
      case d: DefDef =>
        val sym = d.symbol
        if !sym.exists || (sym.is(Flags.Synthetic) && !isDefaultGetter(sym) && !sym.name.toString.contains("$package")) ||
          sym.name.toString.startsWith("copy$default$")
        then ()
        else if sym.owner.isClass && !sym.owner.isAnonymousClass then
          if sym == sym.owner.primaryConstructor then () // its parameters are the fields, seen as such
          else
            withMember(sym) {
              identParams(d)
              traverseChildren(d)
            }
        else traverseChildren(d)
      case v: ValDef =>
        val sym = v.symbol
        if !sym.exists || sym.is(Flags.Synthetic) then ()
        else
          if sym.is(Flags.Param) || sym.owner.isClass then paramType(v)
          if sym.owner.isClass && !sym.owner.isAnonymousClass && !sym.is(Flags.Param) && !sym.is(Flags.ParamAccessor) then
            withMember(sym)(traverseChildren(v))
          else traverseChildren(v)
      case t: TypeTree =>
        for c <- classesIn(t.tpe) do reference(c, t.span, term = false)
      case i: Inlined =>
        if !i.call.isEmpty then traverse(i.call)
        i.bindings.foreach(traverse)
      case b: Block =>
        for case v: ValDef <- b.stats if v.symbol.exists && !v.rhs.isEmpty do locals(v.symbol) = v.rhs
        traverseChildren(b)
      case a: Apply if copyCall(a) => copy(a)
      case u: UnApply => 
        collEscape(u.tpe.widen.classSymbol)
        traverseChildren(u)
      case s @ Select(qual, name) =>
        val q = qual.symbol
        if collectionField(q) then
          val ownerCls = q.owner
          add("colluse", s"${ownerCls.fullName}|${q.name}|$name|$memberName", s.span, List(ownerCls.fullName.toString, q.name.toString, name.toString, memberName))
          qual match
            case Select(inner, _) => traverse(inner)
            case _                => ()
        else
          reference(s.symbol, s.span, term = true)
          fieldUse(s)
          if collectionField(s.symbol) then
            add("colluse", s"${s.symbol.owner.fullName}|${s.symbol.name}|*|$memberName", s.span, List(s.symbol.owner.fullName.toString, s.symbol.name.toString, "*", memberName))
          traverseChildren(s)
      case id: Ident =>
        reference(id.symbol, id.span, term = true)
        if collectionField(id.symbol) then
          add("colluse", s"${id.symbol.owner.fullName}|${id.symbol.name}|*|$memberName", id.span, List(id.symbol.owner.fullName.toString, id.symbol.name.toString, "*", memberName))
      case _ => traverseChildren(tree)

    // ---- domain purity: types of fields and parameters -------------------------------------------------------

    private def paramType(v: ValDef): Unit =
      if member.exists then
        for c <- classesIn(v.tpt.tpe) do reference(c, v.tpt.span, term = false)

    // ---- domain identity -------------------------------------------------------------------------------------

    private def identityClass(tpe: Type): Option[Symbol] =
      classesIn(tpe).find(c => c.isClass && Identity.candidate(c.fullName.toString.stripSuffix("$"), packageOf(c), cfg))

    private val comparing = Set("==", "!=", "equals", "eq", "ne", "contains", "equalsIgnoreCase")

    private def identParams(d: DefDef): Unit =
      val params = d.paramss.flatten.collect { case v: ValDef => v }
      for p <- params do
        identityClass(p.tpt.tpe).foreach { c =>
          val ps = p.symbol
          var compared = false
          if !d.rhs.isEmpty then
            d.rhs.foreachSubTree {
              case Apply(fn, args) if comparing(fn.symbol.name.toString) =>
                val trees = (fn match { case Select(q, _) => List(q); case _ => Nil }) ++ args
                if trees.exists(t => t.existsSubTree(_.symbol == ps)) then compared = true
              case _ => ()
            }
          add(
            "identparam", s"$memberName|${p.name}", p.span,
            List(memberName, p.name.toString, c.fullName.toString.stripSuffix("$"), if ps.isOneOf(Flags.GivenOrImplicit) then "1" else "0", if compared then "1" else "0")
          )
        }

    // ---- class declarations ----------------------------------------------------------------------------------

    private def collectionOfProject(tpe: Type): Option[String] =
      tpe.widenDealias match
        case AppliedType(tycon, args) if tycon.typeSymbol.exists && (tycon.derivesFrom(requiredClass("scala.collection.Iterable")) || tycon.typeSymbol == defn.ArrayClass) =>
          args.flatMap(classesIn).find(c => c.isClass && isProject(packageOf(c)) && !c.is(Flags.Package)).map(_.fullName.toString.stripSuffix("$"))
        case _ => None

    private val collFieldCache = mutable.Map[Symbol, Boolean]()

    private def collectionField(sym: Symbol): Boolean =
      sym.exists && sym.owner.isClass && !sym.owner.is(Flags.ModuleClass) && (sym.is(Flags.ParamAccessor) || sym.is(Flags.CaseAccessor)) &&
        collFieldCache.getOrElseUpdate(sym, sym.owner.isClass && isProject(packageOf(sym)) && collectionOfProject(sym.info.finalResultType).isDefined)

    private def collEscape(cls: Symbol): Unit =
      if cls.exists && cls.is(Flags.CaseClass) && isProject(packageOf(cls)) then
        add("colluse", s"${cls.fullName}|*|unapply|$memberName", Span(0), List(cls.fullName.toString, "*", "unapply", memberName))

    private def classDecl(td: TypeDef): Unit =
      val sym = td.symbol
      if sym.isClass && !sym.is(Flags.ModuleClass) && !sym.is(Flags.Trait) && sym.primaryConstructor.exists then
        val fields = sym.primaryConstructor.paramSymss.flatten.filter(_.isTerm)
        if fields.size >= fieldFloor then
          out += ArchFact(
            "classinfo", source, line(td.span, sym),
            List(sym.fullName.toString, packageOf(sym), if sym.is(Flags.CaseClass) then "case" else "class", fields.size.toString, ArchFact.join(fields.map(_.name.toString)))
          )
        if isProject(packageOf(sym)) then
          for f <- sym.primaryConstructor.paramSymss.flatten.filter(_.isTerm) do
            collectionOfProject(f.info.widenDealias).foreach { elem =>
              val derives = sym.companionModule.exists && sym.companionModule.moduleClass.info.decls.toList.exists(_.is(Flags.Given))
              out += ArchFact("collfield", source, line(td.span, sym), List(sym.fullName.toString, f.name.toString, elem, if derives then "1" else "0"))
            }
          // fields declared in the body are state of the class too
          td.rhs match
            case impl: Template =>
              for v <- impl.body.collect { case v: ValDef if v.symbol.exists && !v.symbol.is(Flags.ParamAccessor) && !v.symbol.is(Flags.Synthetic) => v } do
                identityClass(v.tpt.tpe).foreach { c =>
                  out += ArchFact("identfield", source, line(v.span, sym), List(sym.fullName.toString.stripSuffix("$"), v.name.toString, c.fullName.toString.stripSuffix("$")))
                }
            case _ => ()
        // constructor parameters are fields too
        for f <- sym.primaryConstructor.paramSymss.flatten.filter(_.isTerm) do
          identityClass(f.info.widenDealias).foreach { c =>
            out += ArchFact("identfield", source, line(td.span, sym), List(sym.fullName.toString.stripSuffix("$"), f.name.toString, c.fullName.toString.stripSuffix("$")))
          }

    // ---- god types: field usage ------------------------------------------------------------------------------

    private val fieldCountCache = mutable.Map[Symbol, Int]()

    private def trackedFields(cls: Symbol): Int =
      fieldCountCache.getOrElseUpdate(
        cls,
        if cls.isClass && !cls.is(Flags.ModuleClass) && !cls.is(Flags.JavaDefined) && cls.primaryConstructor.exists && isProject(packageOf(cls)) then
          cls.primaryConstructor.paramSymss.flatten.count(_.isTerm)
        else 0
      )

    private def fieldUse(s: Select): Unit =
      val sym = s.symbol
      if sym.exists && sym.owner.isClass && member.exists && trackedFields(sym.owner) >= fieldFloor then
        if sym.is(Flags.ParamAccessor) || sym.is(Flags.CaseAccessor) then
          add("fielduse", s"$memberPkg|${sym.owner.fullName}|${sym.name}|r", s.span, List(memberName, memberPkg, sym.owner.fullName.toString, sym.name.toString, "r"))
        else if sym.is(Flags.Method) && !sym.isConstructor && !sym.is(Flags.Synthetic) && !sym.isOneOf(Flags.Private) then
          // a method of the class stands for the fields it works on: accessor methods do not hide what is used
          add("fielduse", s"$memberPkg|${sym.owner.fullName}|${sym.name}|m", s.span, List(memberName, memberPkg, sym.owner.fullName.toString, sym.name.toString, "m"))

    // ---- scattered transitions: copy with constants ----------------------------------------------------------

    private def copyTarget(t: Tree): Option[Tree] = t match
      case Apply(fn, _)    => copyTarget(fn)
      case TypeApply(fn, _) => copyTarget(fn)
      case s: Select if s.symbol.exists && s.symbol.name.toString == "copy" && s.symbol.owner.is(Flags.CaseClass) => Some(s)
      case _               => None

    private def copyCall(a: Apply): Boolean = !a.fun.isInstanceOf[Apply] && copyTarget(a.fun).isDefined

    /** Named arguments are wrapped, and the compiler binds the ones it has to reorder to local values first. */
    private def resolve(t: Tree): Tree = t match
      case NamedArg(_, arg) => resolve(arg)
      case Typed(e, _)      => resolve(e)
      case id: Ident if locals.contains(id.symbol) => resolve(locals(id.symbol))
      case _                => t

    private val locals = mutable.Map[Symbol, Tree]()

    private def constant(t: Tree): Option[String] = t match
      case Typed(e, _)    => constant(e)
      case Literal(c)     => if c.tag == Constants.UnitTag then None else Some(c.value.toString match { case s if c.tag == Constants.StringTag => "\"" + s + "\""; case s => s })
      case _: (Ident | Select) if t.symbol.exists && (t.symbol.is(Flags.Module) || t.symbol.is(Flags.Enum)) && t.symbol.isStatic =>
        Some(t.symbol.fullName.toString.replace("$.", ".").stripSuffix("$"))
      case Apply(fn, List(arg)) if fn.symbol.exists && fn.symbol.name.toString == "apply" && fn.symbol.owner.fullName.toString.stripSuffix("$") == "scala.Some" =>
        constant(arg).map(v => s"Some($v)")
      case TypeApply(fn, _) => constant(fn)
      case _              => None

    private def copy(a: Apply): Unit =
      val target = copyTarget(a.fun).get

      val owner = target.symbol.owner
      val params = target.symbol.info.stripPoly match
        case mt: MethodType => mt.paramNames.map(_.toString)
        case _              => Nil
      if params.size == a.args.size && isProject(packageOf(owner)) && member.exists then
        for (p, shown) <- params.zip(a.args); arg = resolve(shown) if !isDefaultGetter(arg.symbol) do
          constant(arg).foreach { v =>
            add("copy", s"$memberName|${owner.fullName}|$p|$v", a.span, List(memberName, topLevelName(member), owner.fullName.toString, p, v))
          }
          if trackedFields(owner) >= fieldFloor then
            add("fielduse", s"$memberPkg|${owner.fullName}|$p|w", a.span, List(memberName, memberPkg, owner.fullName.toString, p, "w"))
      traverseChildren(a)

/** Which types stand for the acting identity or the request context. Judged from names, since the type itself says
  * nothing: an `Utente` that is just a record in the data model is not the actor.
  */
object Identity:
  /** Names that mean "the acting identity or request" wherever the type lives, unless the domain defines it. */
  private val exact = Set(
    "Request", "HttpRequest", "RequestContext", "Context", "Session", "Principal", "Authentication", "Credentials", "Identity",
    "Claims", "Subject", "AuthContext", "SecurityContext", "UserContext", "HttpServerExchange", "Authenticated", "AuthenticatedUser",
    "CurrentUser", "LoggedUser", "UserSession"
  )

  /** Names that are as often plain data (an owner, an author) as the actor. */
  private val weak = Set("User", "Utente", "Actor", "Caller", "Account", "Operatore", "Member")

  private val actorNames = Set(
    "user", "actor", "caller", "currentuser", "principal", "requester", "loggeduser", "utente", "utentecorrente", "operatore",
    "who", "authenticateduser", "currentactor", "actinguser", "loggedinuser"
  )

  def actorName(param: String): Boolean = actorNames(param.toLowerCase)

  private def simple(full: String) = full.split('.').last

  /** A type that could be the identity: recorded at compile time, judged later with the domain packages known. */
  def candidate(full: String, pkg: String, cfg: ArchConfig): Boolean =
    if cfg.identityTypes.nonEmpty then matches(full, cfg.identityTypes)
    else if pkg == "java.security" then simple(full) == "Principal"
    else if isStdLib(pkg) then false
    else
      val n = simple(full)
      exact(n) || weak(n) || PackageKinds.kind(pkg, cfg, internal = true).contains("auth") ||
      n.endsWith("Context") && List("Request", "Auth", "User", "Security", "Session", "Http", "Web").exists(n.stripSuffix("Context").endsWith)

  private def matches(full: String, names: List[String]): Boolean =
    names.exists(n => full == n || full.startsWith(n + ".") || simple(full) == n)

  /** How sure a type is to be the actor: `strong` types always are, `weak` ones only with other evidence. */
  def strength(full: String, pkg: String, cfg: ArchConfig, domain: Set[String]): String =
    if cfg.identityTypes.nonEmpty then "strong"
    else if PackageKinds.kind(pkg, cfg, internal = true).contains("auth") && !domain(pkg) then "strong"
    else if domain(pkg) then "weak"
    else if weak(simple(full)) then "weak"
    else "strong"
