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
  *   source lines the definition spans
  * @param params
  *   explicit value parameters over all parameter lists
  * @param nesting
  *   deepest nesting of branches, loops and closures
  */
final case class MethodFact(
    file: String,
    line: Int,
    sym: String,
    cyclomatic: Int,
    cognitive: Int,
    lines: Int,
    params: Int,
    nesting: Int
)

/** Cohesion of one class or trait with at least two methods.
  *
  * @param clusters
  *   LCOM4: groups of methods that are connected by shared fields or calls; more than one group means the class holds
  *   unrelated responsibilities
  */
final case class ClassFact(file: String, line: Int, sym: String, methods: Int, clusters: List[List[String]])

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
    classDeps: List[ClassDep] = Nil
)

object FileFacts:
  /** Bump whenever extraction changes what the facts mean: older facts cannot be recomputed without recompiling. */
  val version = 4

  private val header = s"layers-facts\t$version"

  /** The source a facts file describes, whatever its version. */
  def peekSource(content: String): Option[String] =
    content.linesIterator.drop(1).nextOption().map(_.split('\t')).collect { case Array("source", source) => source }

  private def clean(s: String): String = s.replace('\t', ' ').replace('\n', ' ')

  def encode(f: FileFacts): String =
    val sb = new StringBuilder
    sb.append(header).append('\n')
    sb.append("source\t").append(clean(f.source)).append('\n')
    for p <- f.packages.toList.sorted do sb.append("pkg\t").append(clean(p)).append('\n')
    for (p, (total, abs)) <- f.types.toList.sortBy(_._1) do
      sb.append("types\t").append(clean(p)).append('\t').append(total).append('\t').append(abs).append('\n')
    for m <- f.methods do
      sb.append("method\t").append(m.line).append('\t').append(clean(m.sym)).append('\t')
        .append(m.cyclomatic).append('\t').append(m.cognitive).append('\t').append(m.lines).append('\t')
        .append(m.params).append('\t').append(m.nesting).append('\n')
    for c <- f.classes do
      val clusters = c.clusters.map(_.map(clean(_).replace(',', '_').replace(';', '_')).mkString(",")).mkString(";")
      sb.append("class\t").append(c.line).append('\t').append(clean(c.sym)).append('\t')
        .append(c.methods).append('\t').append(clusters).append('\n')
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
            case Array("method", line, sym, cyc, cog, lines, params, nest) if ints(line, cyc, cog, lines, params, nest) =>
              MethodFact(source, line.toInt, sym, cyc.toInt, cog.toInt, lines.toInt, params.toInt, nest.toInt)
          }
          val classes = body.collect {
            case Array("class", line, sym, n, clusters) if ints(line, n) =>
              ClassFact(source, line.toInt, sym, n.toInt, clusters.split(";").toList.map(_.split(",").toList))
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
          val wellFormed = body.forall {
            case Array("classdep", line, _, _, _, _, refs) => ints(line, refs)
            case Array("passthrough", line, _, _)        => ints(line)
            case Array("clone", line, _, size, _)        => ints(line, size)
            case Array("typedecl", line, _, _, _, _, _)  => ints(line)
            case Array("method", line, _, cyc, cog, lines, params, nest) => ints(line, cyc, cog, lines, params, nest)
            case Array("class", line, _, n, _)                           => ints(line, n)
            case Array("pkg", _)                    => true
            case Array("types", _, total, abs)      => total.toIntOption.isDefined && abs.toIntOption.isDefined
            case Array("edge", line, _, _, _, _)    => line.toIntOption.isDefined
            case _                                  => false
          }
          if wellFormed then Some(FileFacts(source, packages, edges, types, methods, classes, passThroughs, clones, typeDecls, classDeps)) else None
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
