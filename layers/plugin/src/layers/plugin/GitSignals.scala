package layers.plugin

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

/** What git knows about the code: which lines changed since a revision, which files change often, and which change
  * together. All parsing is separate from running git, so it can be tested on text.
  */
object GitSignals:

  /** Changed line ranges per absolute source path. `None` ranges mean the whole file (new, untracked). */
  final case class Changes(files: Map[String, List[(Int, Int)]], wholeFiles: Set[String]):
    private val real = files.map { case (f, r) => canon(f) -> r }
    private val whole = wholeFiles.map(canon)

    def touches(file: String, ranges: List[(Int, Int)]): Boolean =
      val f = canon(file)
      whole(f) || real.get(f).exists(changed => changed.exists(c => ranges.exists(r => c._1 <= r._2 && r._1 <= c._2)))

  /** The same file may be reached through a symlink (`/var` and `/private/var` on macOS). */
  private def canon(path: String): String =
    try Path.of(path).toRealPath().toString
    catch case _: Exception => path

  object Changes:
    val empty: Changes = Changes(Map.empty, Set.empty)

  private val hunk = """^@@ -\d+(?:,\d+)? \+(\d+)(?:,(\d+))? @@.*""".r

  /** Parses `git diff --unified=0`: the post-image line ranges of every hunk, per file under `root`. */
  def parseDiff(diff: String, root: Path): Map[String, List[(Int, Int)]] =
    var current: Option[String] = None
    val out = collection.mutable.LinkedHashMap[String, List[(Int, Int)]]()
    for line <- diff.linesIterator do
      if line.startsWith("+++ ") then
        current = if line.startsWith("+++ b/") then Some(root.resolve(line.drop(6)).toAbsolutePath.normalize.toString) else None
      else
        line match
          case hunk(start, count) =>
            val n = Option(count).map(_.toInt).getOrElse(1)
            // a pure deletion changes no line of the new file: it touches the line where it happened
            val range = if n == 0 then (start.toInt, start.toInt) else (start.toInt, start.toInt + n - 1)
            current.foreach(f => out(f) = out.getOrElse(f, Nil) :+ range)
          case _ => ()
    out.toMap

  private[plugin] def git(dir: Path, args: String*): Option[String] =
    try
      val pb = new ProcessBuilder((List("git", "-C", dir.toString) ++ args)*)
      pb.redirectErrorStream(false)
      val proc = pb.start()
      val out = new String(proc.getInputStream.readAllBytes(), "UTF-8")
      proc.getErrorStream.readAllBytes()
      if proc.waitFor() == 0 then Some(out) else None
    catch case _: Exception => None

  /** Lines changed since `rev`, including uncommitted edits and untracked files. None when git is unavailable. */
  def changesSince(rev: String, anyFile: Path): Option[Changes] =
    val dir = Option(anyFile.toAbsolutePath.getParent).getOrElse(anyFile)
    for
      top <- git(dir, "rev-parse", "--show-toplevel").map(_.trim).filter(_.nonEmpty)
      diff <- git(Path.of(top), "diff", "--unified=0", "--no-color", "--no-ext-diff", rev, "--")
    yield
      val root = Path.of(top)
      val untracked = git(root, "ls-files", "--others", "--exclude-standard").getOrElse("").linesIterator
        .filter(_.nonEmpty).map(f => root.resolve(f).toAbsolutePath.normalize.toString).toSet
      Changes(parseDiff(diff, root), untracked)

  /** Commits touching each file in the history, one set of files per commit. */
  def parseLog(log: String): List[Set[String]] =
    log.split('\u0000').toList.map(_.linesIterator.map(_.trim).filter(_.nonEmpty).toSet).filter(_.nonEmpty)

  def history(since: String, anyFile: Path): Option[(Path, List[Set[String]])] =
    val dir = Option(anyFile.toAbsolutePath.getParent).getOrElse(anyFile)
    for
      top <- git(dir, "rev-parse", "--show-toplevel").map(_.trim).filter(_.nonEmpty)
      log <- git(Path.of(top), ("log" :: windowArgs(since) ++ List("--name-only", "--no-merges", "--pretty=format:%x00"))*)
    yield (Path.of(top), parseLog(log))

  private val commitCount = """(\d+)\s+commits?""".r

  /** `500 commits` limits the log to the last 500 commits (stable: it does not move with the calendar); anything else
    * is a date for `git log --since`, such as `1 year ago`.
    */
  private[plugin] def windowArgs(spec: String): List[String] = spec.trim match
    case commitCount(n) => List("-n", n)
    case other          => List(s"--since=$other")

  /** Number of commits per file. */
  def churn(commits: List[Set[String]]): Map[String, Int] =
    commits.flatten.groupBy(identity).view.mapValues(_.size).toMap

  /** Files that change together: pairs with at least `minCommits` joint commits, in at least `minRatio` of the
    * commits of the less changed one. Commits touching more than `maxFilesPerCommit` files (mass renames, formatting)
    * say nothing about coupling and are skipped.
    */
  final case class CoChange(a: String, b: String, together: Int, ratio: Double)

  def coChange(commits: List[Set[String]], minCommits: Int = 4, minRatio: Double = 0.6, maxFilesPerCommit: Int = 25): List[CoChange] =
    val usable = commits.filter(_.size <= maxFilesPerCommit)
    val each = churn(usable)
    val pairs = collection.mutable.Map[(String, String), Int]().withDefaultValue(0)
    for c <- usable do
      val fs = c.toList.sorted
      for i <- fs.indices; j <- i + 1 until fs.size do pairs((fs(i), fs(j))) += 1
    pairs.toList.collect {
      case ((a, b), n) if n >= minCommits && n.toDouble / math.min(each(a), each(b)) >= minRatio =>
        CoChange(a, b, n, n.toDouble / math.min(each(a), each(b)))
    }.sortBy(c => (-c.together, c.a, c.b))

  /** Complexity in files that change a lot: where an hour of cleaning pays back. */
  final case class Hotspot(file: String, commits: Int, complexity: Int):
    def score: Int = commits * complexity

  def hotspots(churn: Map[String, Int], complexityByFile: Map[String, Int], top: Int = 10): List[Hotspot] =
    complexityByFile.toList.flatMap { case (f, c) => churn.get(f).map(n => Hotspot(f, n, c)) }
      .filter(h => h.complexity > 0).sortBy(h => (-h.score, h.file)).take(top)
