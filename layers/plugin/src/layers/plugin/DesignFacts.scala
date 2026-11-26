package layers.plugin

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.security.MessageDigest

import scala.jdk.CollectionConverters.*

/** A cross-package reference found in a source file: `fromSym` (in `fromPkg`) uses `toSym` (in `toPkg`). */
final case class Edge(file: String, line: Int, fromPkg: String, fromSym: String, toPkg: String, toSym: String)

/** Top-level class `fromTop` uses top-level class `toTop`, in the same package or not. Top-level classes are the unit
  * that moves when code is moved between packages.
  *
  * @param refs
  *   source lines in `file` where the use happens
  */
final case class ClassDep(
    file: String,
    line: Int,
    fromTop: String,
    toTop: String,
    fromPkg: String,
    toPkg: String,
    refs: Int
)

/** Raw measurements of one method, kept unevaluated so thresholds can change without recompiling.
  *
  * @param lines
  *   source lines the definition spans (used to point at the code, never to judge it)
  * @param size
  *   logical lines: non-blank code characters (comments left out) at 30 per line, so reformatting cannot change it
  * @param params
  *   explicit value parameters over all parameter lists
  * @param nesting
  *   deepest nesting of branches, loops and closures
  * @param pkg
  *   package the method is declared in: a call across packages is a module boundary, not a method split
  * @param entry
  *   overrides an inherited member: it is called through its parent type, so the callers seen here are not all of them
  */
final case class MethodFact(
    file: String,
    line: Int,
    sym: String,
    cyclomatic: Int,
    cognitive: Int,
    lines: Int,
    size: Int,
    params: Int,
    nesting: Int,
    priv: Boolean = false,
    calls: List[String] = Nil,
    entry: Boolean = false,
    pkg: String = ""
)

/** One method of a class, as far as cohesion is concerned.
  *
  * @param types
  *   domain types (outermost class names) in the signature: what the method works on
  * @param calls
  *   sibling methods it calls
  * @param fields
  *   fields of the class it reads or writes
  */
final case class ClassMethod(name: String, types: List[String], calls: List[String], fields: List[String])

/** What the cohesion and facade checks need to know about one class or trait.
  *
  * @param exports
  *   members that are only `export` forwarders, which say nothing about what the class itself does
  */
final case class ClassFact(file: String, line: Int, sym: String, methods: List[ClassMethod], exports: Int = 0)

/** A public-looking method that only forwards its parameters, unchanged and in order, to a method of another class. */
final case class PassThroughFact(file: String, line: Int, sym: String, target: String)

/** Structural fingerprint of a method: identical `hash` means the same code up to local names and literal values.
  *
  * @param size
  *   number of syntax tree nodes
  */
final case class CloneFact(file: String, line: Int, sym: String, size: Int, hash: String)

/** A declared class, trait or object and what it extends.
  *
  * @param kind
  *   trait, abstract, class, object or anon
  * @param sealedType
  *   sealed types are closed hierarchies, not extension points
  * @param generic
  *   has type parameters (type classes and generic containers are extension points)
  * @param local
  *   declared inside a method
  * @param parents
  *   direct supertypes that are not from the standard library
  */
final case class TypeDecl(
    file: String,
    line: Int,
    sym: String,
    pkg: String,
    kind: String,
    sealedType: Boolean,
    generic: Boolean,
    local: Boolean,
    parents: List[String]
)

/** A case class that could be a bundle of parameters.
  *
  * @param fields
  *   constructor parameters
  * @param hasBehaviour
  *   declares methods or state beyond the fields
  * @param derives
  *   a serialisation or typeclass instance is derived, so it is data, not a bundle
  */
final case class BundleDecl(file: String, line: Int, sym: String, fields: Int, hasBehaviour: Boolean, derives: Boolean)

/** Where a case class is built (`kind` = "new") or taken as an explicit parameter by method `from` (`kind` = "param"). */
final case class BundleUse(file: String, line: Int, kind: String, sym: String, from: String)

/** Everything the design checks need to know about one source file, independent of the compiler run that saw it.
  *
  * @param source
  *   absolute path of the source file
  * @param packages
  *   packages that have members declared in this file
  * @param edges
  *   cross-package references, one per distinct (fromSym, toSym)
  * @param types
  *   per package, (all classes and traits, abstract ones) declared in this file
  * @param methods
  *   measurements of the methods declared in this file
  * @param classes
  *   cohesion of the classes declared in this file
  * @param passThroughs
  *   methods that only forward to another class
  * @param clones
  *   fingerprints of the larger methods declared in this file
  * @param typeDecls
  *   classes, traits and objects declared in this file
  * @param classDeps
  *   which top-level class uses which, including within a package
  * @param bundles
  *   case classes that could be parameter bundles
  * @param bundleUses
  *   where those and other case classes are built and passed
  * @param lines
  *   length of the source file
  */
final case class FileFacts(
    source: String,
    packages: Set[String],
    edges: List[Edge],
    types: Map[String, (Int, Int)] = Map.empty,
    methods: List[MethodFact] = Nil,
    classes: List[ClassFact] = Nil,
    passThroughs: List[PassThroughFact] = Nil,
    clones: List[CloneFact] = Nil,
    typeDecls: List[TypeDecl] = Nil,
    classDeps: List[ClassDep] = Nil,
    bundles: List[BundleDecl] = Nil,
    bundleUses: List[BundleUse] = Nil,
    lines: Int = 0,
    arch: List[ArchFact] = Nil
)

object FileFacts:
  /** Bump whenever extraction changes what the facts mean: older facts cannot be recomputed without recompiling. */
  val version = 10

  private val header = s"layers-facts\t$version"

  /** The source a facts file describes, whatever its version. */
  def peekSource(content: String): Option[String] =
    content.linesIterator.drop(1).nextOption().map(_.split('\t')).collect { case Array("source", source) => source }

  private def clean(s: String): String = s.replace('\t', ' ').replace('\n', ' ')

  private def listItem(s: String): String = clean(s).replace(',', '_').replace('|', '_').replace(';', '_')

  private def list(s: String): List[String] = if s.isEmpty then Nil else s.split(",", -1).toList

  def encode(f: FileFacts): String =
    val sb = new StringBuilder
    sb.append(header).append('\n')
    sb.append("source\t").append(clean(f.source)).append('\n')
    for p <- f.packages.toList.sorted do sb.append("pkg\t").append(clean(p)).append('\n')
    for (p, (total, abs)) <- f.types.toList.sortBy(_._1) do
      sb.append("types\t").append(clean(p)).append('\t').append(total).append('\t').append(abs).append('\n')
    for m <- f.methods do
      sb.append("method\t").append(m.line).append('\t').append(clean(m.sym)).append('\t')
        .append(m.cyclomatic).append('\t').append(m.cognitive).append('\t').append(m.lines).append('\t').append(m.size).append('\t')
        .append(m.params).append('\t').append(m.nesting).append('\t').append((if m.priv then 1 else 0) + (if m.entry then 2 else 0)).append('\t')
        .append(m.calls.map(listItem).mkString(",")).append('\t').append(clean(m.pkg)).append('\n')
    for c <- f.classes do
      val methods = c.methods.map { m =>
        List(m.name, m.types.map(listItem).mkString(","), m.calls.map(listItem).mkString(","), m.fields.map(listItem).mkString(","))
          .map(x => x.replace('|', '_').replace(';', '_')).mkString("|")
      }.mkString(";")
      sb.append("class\t").append(c.line).append('\t').append(clean(c.sym)).append('\t')
        .append(c.exports).append('\t').append(methods).append('\n')
    for x <- f.passThroughs do
      sb.append("passthrough\t").append(x.line).append('\t').append(clean(x.sym)).append('\t').append(clean(x.target)).append('\n')
    for x <- f.clones do
      sb.append("clone\t").append(x.line).append('\t').append(clean(x.sym)).append('\t').append(x.size).append('\t').append(x.hash).append('\n')
    for x <- f.typeDecls do
      val flags = List("sealed" -> x.sealedType, "generic" -> x.generic, "local" -> x.local).collect { case (n, true) => n }
      sb.append("typedecl\t").append(x.line).append('\t').append(clean(x.sym)).append('\t').append(clean(x.pkg)).append('\t')
        .append(x.kind).append('\t').append(if flags.isEmpty then "-" else flags.mkString(",")).append('\t')
        .append(x.parents.map(clean).mkString(",")).append('\n')
    for x <- f.classDeps do
      sb.append("classdep\t").append(x.line).append('\t').append(clean(x.fromTop)).append('\t').append(clean(x.toTop)).append('\t')
        .append(clean(x.fromPkg)).append('\t').append(clean(x.toPkg)).append('\t').append(x.refs).append('\n')
    sb.append("lines\t").append(f.lines).append('\n')
    for x <- f.arch do sb.append(ArchFact.encode(x)).append('\n')
    for x <- f.bundles do
      sb.append("bundle\t").append(x.line).append('\t').append(clean(x.sym)).append('\t').append(x.fields).append('\t')
        .append(if x.hasBehaviour then 1 else 0).append('\t').append(if x.derives then 1 else 0).append('\n')
    for x <- f.bundleUses do
      sb.append("bundleuse\t").append(x.line).append('\t').append(x.kind).append('\t').append(clean(x.sym)).append('\t')
        .append(clean(x.from)).append('\n')
    for e <- f.edges do
      sb.append("edge\t").append(e.line).append('\t')
        .append(clean(e.fromPkg)).append('\t').append(clean(e.fromSym)).append('\t')
        .append(clean(e.toPkg)).append('\t').append(clean(e.toSym)).append('\n')
    sb.toString

  /** None when the content is not a facts file of the current version. */
  def decode(content: String): Option[FileFacts] =
    val lines = content.linesIterator.toList
    lines match
      case h :: rest if h == header =>
        rest.headOption.map(_.split('\t')).collect { case Array("source", source) => source }.flatMap { source =>
          val body = rest.tail.map(_.split("\t", -1))
          val packages = body.collect { case Array("pkg", p) => p }.toSet
          val edges = body.collect {
            case Array("edge", line, fp, fs, tp, ts) if line.toIntOption.isDefined =>
              Edge(source, line.toInt, fp, fs, tp, ts)
          }
          val types = body.collect {
            case Array("types", p, total, abs) if total.toIntOption.isDefined && abs.toIntOption.isDefined =>
              p -> (total.toInt, abs.toInt)
          }.toMap
          def ints(xs: String*) = xs.forall(_.toIntOption.isDefined)
          val methods = body.collect {
            case Array("method", line, sym, cyc, cog, lines, size, params, nest, priv, calls, pkg)
                if ints(line, cyc, cog, lines, size, params, nest, priv) =>
              val flags = priv.toInt
              MethodFact(source, line.toInt, sym, cyc.toInt, cog.toInt, lines.toInt, size.toInt, params.toInt, nest.toInt, (flags & 1) != 0, list(calls), (flags & 2) != 0, pkg)
          }
          val classes = body.collect {
            case Array("class", line, sym, exports, methods) if ints(line, exports) =>
              val ms = if methods.isEmpty then Nil else methods.split(";", -1).toList.map(_.split("\\|", -1)).collect {
                case Array(name, types, calls, fields) => ClassMethod(name, list(types), list(calls), list(fields))
              }
              ClassFact(source, line.toInt, sym, ms, exports.toInt)
          }
          val passThroughs = body.collect {
            case Array("passthrough", line, sym, target) if ints(line) => PassThroughFact(source, line.toInt, sym, target)
          }
          val clones = body.collect {
            case Array("clone", line, sym, size, hash) if ints(line, size) => CloneFact(source, line.toInt, sym, size.toInt, hash)
          }
          val typeDecls = body.collect {
            case Array("typedecl", line, sym, pkg, kind, flags, parents) if ints(line) =>
              val fs = flags.split(",").toSet
              TypeDecl(
                source, line.toInt, sym, pkg, kind, fs("sealed"), fs("generic"), fs("local"),
                if parents.isEmpty then Nil else parents.split(",").toList
              )
          }
          val classDeps = body.collect {
            case Array("classdep", line, ft, tt, fp, tp, refs) if ints(line, refs) =>
              ClassDep(source, line.toInt, ft, tt, fp, tp, refs.toInt)
          }
          val bundles = body.collect {
            case Array("bundle", line, sym, fields, behaviour, derives) if ints(line, fields) =>
              BundleDecl(source, line.toInt, sym, fields.toInt, behaviour == "1", derives == "1")
          }
          val bundleUses = body.collect {
            case Array("bundleuse", line, kind, sym, from) if ints(line) => BundleUse(source, line.toInt, kind, sym, from)
          }
          val arch = body.flatMap(ArchFact.decode(source, _))
          val lineCount = body.collectFirst { case Array("lines", n) if n.toIntOption.isDefined => n.toInt }.getOrElse(0)
          val wellFormed = body.forall {
            case Array("lines", n) => n.toIntOption.isDefined
            case Array("bundle", line, _, fields, _, _) => ints(line, fields)
            case Array("bundleuse", line, _, _, _)      => ints(line)
            case Array("classdep", line, _, _, _, _, refs) => ints(line, refs)
            case Array("passthrough", line, _, _)        => ints(line)
            case Array("clone", line, _, size, _)        => ints(line, size)
            case Array("typedecl", line, _, _, _, _, _)  => ints(line)
            case Array("method", line, _, cyc, cog, lines, size, params, nest, flags, _, _) => ints(line, cyc, cog, lines, size, params, nest, flags)
            case Array("class", line, _, exports, _)                           => ints(line, exports)
            case Array("arch", _, line, _*)         => line.toIntOption.isDefined
            case Array("pkg", _)                    => true
            case Array("types", _, total, abs)      => total.toIntOption.isDefined && abs.toIntOption.isDefined
            case Array("edge", line, _, _, _, _)    => line.toIntOption.isDefined
            case _                                  => false
          }
          if wellFormed then Some(FileFacts(source, packages, edges, types, methods, classes, passThroughs, clones, typeDecls, classDeps, bundles, bundleUses, lineCount, arch)) else None
        }
      case _ => None

/** Per-source facts persisted next to the compiled classes, so a partial (incremental) compilation can still be
  * analysed against the whole project. The compiler only sees the files it recompiles; facts of untouched files come
  * from here.
  */
final class FactsStore(dir: Path):
  import FactsStore.Loaded

  private def fileFor(source: String): Path =
    val digest = MessageDigest.getInstance("SHA-256").digest(source.getBytes(StandardCharsets.UTF_8))
    dir.resolve(digest.take(8).map(b => "%02x".format(b & 0xff)).mkString + ".tsv")

  def write(facts: FileFacts): Unit =
    Files.createDirectories(dir)
    val target = fileFor(facts.source)
    val tmp = Files.createTempFile(dir, "facts", ".tmp")
    Files.writeString(tmp, FileFacts.encode(facts), StandardCharsets.UTF_8)
    Files.move(tmp, target, StandardCopyOption.REPLACE_EXISTING)

  /** Facts of deleted sources are removed. */
  def loadOthers(exclude: Set[String]): Loaded =
    if !Files.isDirectory(dir) then Loaded(Nil, Nil)
    else
      val files = Files.list(dir).iterator.asScala.filter(_.getFileName.toString.endsWith(".tsv")).toList.sorted
      val read = files.map { f =>
        val content = Files.readString(f, StandardCharsets.UTF_8)
        (f, content, FileFacts.decode(content))
      }
      val facts = List.newBuilder[FileFacts]
      val outdated = List.newBuilder[String]
      for (f, content, decoded) <- read do
        val source = decoded.map(_.source).orElse(FileFacts.peekSource(content))
        source match
          case Some(src) if Files.exists(Path.of(src)) =>
            if !exclude(src) then
              decoded match
                case Some(d) => facts += d
                case None    => outdated += src
          case _ => Files.deleteIfExists(f)
      Loaded(facts.result(), outdated.result())

object FactsStore:
  /** @param facts
    *   facts of all sources not in `exclude`
    * @param outdated
    *   sources that still exist but whose facts were written by another plugin version or are unreadable
    */
  final case class Loaded(facts: List[FileFacts], outdated: List[String])
