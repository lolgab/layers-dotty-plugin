package layers.plugin

import java.nio.file.Path

import scala.collection.mutable

/** Things that change together belong together (Derek Comartin): a feature spread over packages makes every change a
  * cross-package change. Measured on git history, so it is only checked when `-P:layers:history=<window>` is given.
  *
  * Findings:
  *   - `co-change <a> <-> <b>`: two packages that are changed in the same commit far more often than chance explains
  *   - `feature-spread <noun>`: a domain noun in the names of files of at least `minFeatureLayers` packages, whose
  *     files are changed together; the finding proposes the vertical slice that would hold them
  *
  * Noise is controlled in the data, not in the message: commits that touch more than `maxCommitFiles` files (mass
  * renames, reformatting, giant refactors) are ignored, a rename is followed so that moving a file moves its history,
  * and a pair counts only when it co-changes more often than the base rate of two busy packages would give
  * (`minLift`): a package touched by 80% of all commits co-changes with everything.
  */
object ChangeScatter:

  /** @param window
    *   `500 commits` or any date `git log --since` takes (`1 year ago`)
    * @param minCoChange
    *   share of the commits of the less changed package (or of the feature's files) that must also touch the other
    * @param minCommits
    *   fewest commits that touch both, for a pair or a feature
    * @param minPairCommits
    *   a package pair is reported only when some single file pair co-changes in at least this many commits: coupling
    *   spread over many file pairs is diffuse (every feature touching a wiring file), not one capability split in two
    * @param minPairShare
    *   ... and that file pair accounts for at least this share of the commits that touch both packages
    * @param wiringPackages
    *   a file that changed together with files of at least this many other packages is a wiring file (the aggregator
    *   every feature touches): coupling to it says nothing about two packages, so its pairs do not count as concentrated
    * @param minLift
    *   how many times more often than independent changes would the two packages change together
    * @param maxCommitFiles
    *   commits touching more files than this are ignored
    * @param maxCommitPackages
    *   commits touching more packages than this are cross-cutting maintenance (adding a plugin, reformatting, a rename
    *   of a shared concept), not one capability changing, and are ignored
    * @param minFeatureLayers
    *   packages a noun must appear in to be a spread feature
    * @param minUsable
    *   fewest usable commits for the history to say anything; below that the check is skipped with a warning
    * @param stopWords
    *   words that name a technical role, not a business concept (`Service`, `Repository`), plus the project's own
    */
  final case class Options(
      window: String,
      minCoChange: Double = 0.4,
      minCommits: Int = 8,
      minLift: Double = 1.5,
      maxCommitFiles: Int = 30,
      maxCommitPackages: Int = 6,
      minFeatureLayers: Int = 3,
      minUsable: Int = 20,
      stopWords: Set[String] = defaultStopWords,
      minPairCommits: Int = 4,
      minPairShare: Double = 0.25,
      wiringPackages: Int = 10
  )

  val defaultStopWords: Set[String] = Set(
    "service", "services", "repository", "repo", "store", "stores", "handler", "handlers", "route", "routes", "router",
    "helper", "helpers", "util", "utils", "manager", "controller", "view", "views", "render", "renders", "rendering",
    "model", "models", "data", "base", "impl", "form", "forms", "page", "pages", "client", "parser", "builder", "factory",
    "mapper", "validator", "request", "response", "result", "results", "error", "errors", "main", "layer", "common", "cache",
    "config", "settings", "job", "jobs", "test", "spec", "input", "dto", "json", "extensions", "given", "with", "from"
  )

  def options(opts: List[String]): Option[Options] =
    def value(name: String) = opts.collectFirst { case o if o.startsWith(name + "=") => o.drop(name.length + 1).trim }
    value("history").filter(_.nonEmpty).map { window =>
      val d = Options(window)
      def int(name: String, default: Int) = value(name).flatMap(_.toIntOption).filter(_ > 0).getOrElse(default)
      def pct(name: String, default: Double) =
        value(name).flatMap(_.toDoubleOption).map(x => if x > 1 then x / 100 else x).filter(x => x > 0 && x <= 1).getOrElse(default)
      Options(
        window,
        pct("minCoChange", d.minCoChange),
        int("minCoChangeCommits", d.minCommits),
        value("minCoChangeLift").flatMap(_.toDoubleOption).filter(_ > 0).getOrElse(d.minLift),
        int("maxCommitFiles", d.maxCommitFiles),
        int("maxCommitPackages", d.maxCommitPackages),
        int("minFeatureLayers", d.minFeatureLayers),
        d.minUsable,
        d.stopWords ++ value("featureStopWords").toList.flatMap(_.split("[,;]")).map(_.trim.toLowerCase).filter(_.nonEmpty),
        int("minPairCommits", d.minPairCommits),
        pct("minPairShare", d.minPairShare),
        int("wiringPackages", d.wiringPackages)
      )
    }

  /** One commit: the files it changed (renames followed to their current path) and how many files it touched in all. */
  final case class Commit(files: Set[String], size: Int)

  /** @param findings
    *   what failed
    * @param warnings
    *   why the check could not run (git missing, shallow history, too little history); the build does not fail
    * @param note
    *   how many commits the verdict is based on
    * @param checked
    *   false when the check did not run, so baseline entries of its kinds are left alone
    */
  final case class Result(findings: List[Finding], warnings: List[String], note: Option[String], checked: Boolean)

  val skipped: Result = Result(Nil, Nil, None, checked = false)

  /** Baseline keys of the findings this check produces. */
  def isHistoryKey(key: String): Boolean = key.startsWith("co-change ") || key.startsWith("feature-spread ")

  // ---------------------------------------------------------------- history

  /** `git log --name-status` in the order git prints it (newest first). A rename `old -> new` makes older mentions of
    * `old` mean `new`, so history follows a file that was moved. Mass commits still contribute their renames.
    */
  def parseStatusLog(log: String): List[Commit] =
    val alias = mutable.Map[String, String]()
    def now(p: String) = alias.getOrElse(p, p)
    log.split('\u0000').toList.flatMap { block =>
      val files = mutable.Set[String]()
      var size = 0
      for line <- block.linesIterator.map(_.trim).filter(_.nonEmpty) do
        line.split('\t').toList match
          case status :: old :: renamed :: Nil if status.startsWith("R") || status.startsWith("C") =>
            size += 1
            val current = now(renamed)
            if status.startsWith("R") then alias(old) = current
            files += current
          case _ :: path :: Nil =>
            size += 1
            files += now(path)
          case _ => ()
      Option.when(size > 0)(Commit(files.toSet, size))
    }

  private def canon(path: String): String =
    try Path.of(path).toRealPath().toString
    catch case _: Exception => Path.of(path).toAbsolutePath.normalize.toString

  /** None with the reason when the history cannot be used. */
  def load(window: String, anyFile: Path): Either[String, List[Commit]] =
    val dir = Option(anyFile.toAbsolutePath.getParent).getOrElse(anyFile)
    GitSignals.git(dir, "rev-parse", "--show-toplevel").map(_.trim).filter(_.nonEmpty) match
      case None => Left("git is not available here")
      case Some(top) =>
        if GitSignals.git(Path.of(top), "rev-parse", "--is-shallow-repository").exists(_.trim == "true") then
          Left("the git history is shallow (a clone with limited depth), so co-change would be measured on a fragment")
        else
          val args = List("-c", "core.quotePath=false", "log") ++ GitSignals.windowArgs(window) ++
            List("--name-status", "-M", "--no-merges", "--pretty=format:%x00")
          GitSignals.git(Path.of(top), args*) match
            case None => Left("git log failed")
            case Some(log) =>
              val real = canon(top)
              Right(parseStatusLog(log).map(c => c.copy(files = c.files.map(f => Path.of(real).resolve(f).normalize.toString))))

  /** Reads the history and analyses it; the only part that touches git. */
  def run(opts: List[String], facts: List[FileFacts], graph: PackageGraph): Result =
    options(opts) match
      case None => skipped
      case Some(o) =>
        facts.headOption.map(f => Path.of(f.source)) match
          case None => skipped
          case Some(any) =>
            load(o.window, any) match
              case Left(reason) =>
                Result(Nil, List(s"[layers] co-change: skipped, $reason"), None, checked = false)
              case Right(commits) =>
                val canonical = facts.map(f => f.copy(source = canon(f.source)))
                analyze(commits, canonical, graph, o)

  // ---------------------------------------------------------------- analysis

  /** Package of every known file; a file that no longer exists gets the package of the one current file of the same name
    * (moved and rewritten), else the package its directory names, if it is one.
    */
  private final class FilePackages(facts: List[FileFacts], graph: PackageGraph):
    private val known = facts.map(f => f.source -> f.packages).toMap
    // source roots: where a file with package `a.b` in directory `.../a/b` starts
    private val roots: List[String] = facts.flatMap { f =>
      f.packages.toList match
        case List(p) =>
          val dir = Option(Path.of(f.source).getParent).map(_.toString).getOrElse("")
          val suffix = "/" + p.replace('.', '/')
          Option.when(dir.endsWith(suffix))(dir.dropRight(suffix.length))
        case _ => None
    }.distinct.sortBy(r => (-r.length, r))
    private val nodes = graph.nodes.toSet
    private val cache = mutable.Map[String, Set[String]]()

    // A file that was moved and rewritten in one commit is not a rename to git (too little in common): its history is
    // still the history of the one current file of that name.
    private val byName: Map[String, Set[String]] =
      facts.groupBy(f => Path.of(f.source).getFileName.toString).collect { case (n, fs) if fs.size == 1 && fs.head.packages.nonEmpty => n -> fs.head.packages }

    def of(file: String): Set[String] =
      cache.getOrElseUpdate(file, known.getOrElse(file, byName.getOrElse(Path.of(file).getFileName.toString, inferred(file))))

    private def inferred(file: String): Set[String] =
      if !file.endsWith(".scala") then Set.empty
      else
        roots.find(r => file.startsWith(r + "/")).flatMap { r =>
          val dir = Option(Path.of(file).getParent).map(_.toString).getOrElse("")
          val pkg = dir.drop(r.length + 1).replace('/', '.')
          Option.when(nodes(pkg))(pkg)
        }.toSet

  private def pct(x: Double) = math.round(x * 100).toInt

  def analyze(allCommits: List[Commit], facts: List[FileFacts], graph: PackageGraph, o: Options): Result =
    val usable = allCommits.filter(_.size <= o.maxCommitFiles)
    val pkgs = new FilePackages(facts, graph)
    val located = usable.map(c => c -> c.files.flatMap(pkgs.of)).filter(_._2.nonEmpty)
    val narrow = located.filter(_._2.size <= o.maxCommitPackages)
    val summary =
      s"${narrow.size} commits used of ${allCommits.size} in ${o.window}; ignored: ${allCommits.size - usable.size} touching more than " +
        s"${o.maxCommitFiles} files, ${located.size - narrow.size} touching more than ${o.maxCommitPackages} packages, " +
        s"${usable.size - located.size} touching no file known now"
    val note = s"[layers] co-change: $summary"
    if narrow.size < o.minUsable then
      Result(
        Nil,
        List(s"[layers] co-change: skipped, only ${narrow.size} usable commits in the window (at least ${o.minUsable} needed). $summary"),
        None, checked = false
      )
    else
      val used = mutable.Set[(String, Int)]()
      val (pairs, diffuse) = packagePairs(narrow, facts, graph, o, summary, used, pkgs)
      val (features, unproven) = featureSpread(narrow.map(_._1), facts, graph, pkgs, o, summary, used)
      val hint =
        if unproven.isEmpty then ""
        else
          s"\n[layers] co-change: ${unproven.size} nouns appear in the file names of at least ${o.minFeatureLayers} packages but their files have too little " +
            s"shared history to judge (files created or moved recently): ${unproven.take(8).mkString(", ")}${if unproven.size > 8 then ", ..." else ""}"
      val spread =
        if diffuse.isEmpty then ""
        else
          s"\n[layers] co-change: ${diffuse.size} package pairs change together only diffusely (no file pair in at least ${o.minPairCommits} commits and ${pct(o.minPairShare)}% of the joint ones), not reported: " +
            diffuse.take(6).map { case (a, b, k, top) => s"$a <-> $b ($k commits, best file pair $top)" }.mkString(", ") + (if diffuse.size > 6 then ", ..." else "")
      Result((pairs ++ features).sortBy(f => (f.file, f.line, f.key)), Nil, Some(note + hint + spread), checked = true)

  // ---------------------------------------------------------------- package pairs

  /** Where to report a package-level finding: a file that exists, at a position no other finding of this check uses (the
    * compiler shows only one error per position, so findings that share one would hide each other).
    */
  private def place(candidates: List[String], used: mutable.Set[(String, Int)]): (String, Int) =
    val files = candidates.distinct
    val spot = files.map(f => (f, 1)).find(p => !used(p)).orElse(files.headOption.map { f =>
      (f, Iterator.from(2).find(l => !used((f, l))).get)
    }).getOrElse(("", 1))
    used += spot
    spot

  private def packagePairs(
      commits: List[(Commit, Set[String])], facts: List[FileFacts], graph: PackageGraph, o: Options, summary: String,
      used: mutable.Set[(String, Int)], pkgs: FilePackages
  ): (List[Finding], List[(String, String, Int, Int)]) =
    val n = commits.size
    val each = mutable.Map[String, Int]().withDefaultValue(0)
    val both = mutable.Map[(String, String), Int]().withDefaultValue(0)
    for (_, ps) <- commits do
      val sorted = ps.toList.sorted
      sorted.foreach(p => each(p) += 1)
      for i <- sorted.indices; j <- i + 1 until sorted.size do both((sorted(i), sorted(j))) += 1
    val related = Slices.families(graph.nodes)
    val flagged = both.toList.sortBy(_._1).flatMap { case ((a, b), together) =>
      val less = math.min(each(a), each(b))
      val ratio = together.toDouble / less
      val lift = together.toDouble * n / (each(a).toDouble * each(b))
      // A package and the only package that uses it change together because one is the other's detail.
      val sole = (graph.sites.contains((a, b)) && graph.afferent(b) == 1) || (graph.sites.contains((b, a)) && graph.afferent(a) == 1)
      Option.when(together >= o.minCommits && ratio >= o.minCoChange && lift >= o.minLift && !related(a, b) && !sole)(
        (a, b, together, ratio, lift, each(a), each(b))
      )
    }
    // a file changed together with files of many packages is wiring: it changes for every feature
    val touched = mutable.Map[String, mutable.Set[String]]()
    for (c, ps) <- commits; f <- c.files do touched.getOrElseUpdate(f, mutable.Set()) ++= ps
    def wiring(f: String) = (touched.getOrElse(f, Set.empty[String]) -- pkgs.of(f)).size >= o.wiringPackages
    val judged = flagged.map { case x @ (a, b, together, _, _, _, _) =>
      (x, jointFiles(a, b, commits, pkgs).filterNot { case ((fa, fb), _) => wiring(fa) || wiring(fb) })
    }
    def concentrated(together: Int, joint: List[((String, String), Int)]) =
      joint.headOption.exists { case (_, k) => k >= o.minPairCommits && k.toDouble / together >= o.minPairShare - 1e-9 }
    val findings = judged.collect {
      case ((a, b, together, ratio, lift, ea, eb), joint) if concentrated(together, joint) =>
        pairFinding(a, b, together, ratio, lift, ea, eb, joint, commits, facts, graph, summary, used)
    }
    val diffuse = judged.collect {
      case ((a, b, together, _, _, _, _), joint) if !concentrated(together, joint) => (a, b, together, joint.headOption.map(_._2).getOrElse(0))
    }
    (findings, diffuse)

  /** For two packages, how often each pair of their files changed in the same commit, most often first. */
  private def jointFiles(
      a: String, b: String, commits: List[(Commit, Set[String])], pkgs: FilePackages
  ): List[((String, String), Int)] =
    def inPkg(f: String, p: String) = pkgs.of(f)(p)
    val joint = mutable.Map[(String, String), Int]().withDefaultValue(0)
    for (c, ps) <- commits if ps(a) && ps(b) do
      for fa <- c.files.filter(inPkg(_, a)).toList.sorted; fb <- c.files.filter(inPkg(_, b)).toList.sorted do joint((fa, fb)) += 1
    joint.toList.sortBy { case ((fa, fb), k) => (-k, fa, fb) }

  private def pairFinding(
      a: String, b: String, together: Int, ratio: Double, lift: Double, ea: Int, eb: Int,
      joint: List[((String, String), Int)],
      commits: List[(Commit, Set[String])], facts: List[FileFacts], graph: PackageGraph, summary: String,
      used: mutable.Set[(String, Int)]
  ): Finding =
    val fileOf = facts.flatMap(f => f.packages.map(f.source -> _)).groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
    def inPkg(f: String, p: String) = fileOf.get(f).exists(_(p))
    val top = joint.take(4)
    val churnA = commits.flatMap(_._1.files).filter(inPkg(_, a)).groupBy(identity).view.mapValues(_.size).toMap
    val churnB = commits.flatMap(_._1.files).filter(inPkg(_, b)).groupBy(identity).view.mapValues(_.size).toMap
    def byChurn(c: Map[String, Int]) = c.toList.sortBy(x => (-x._2, x._1)).map(_._1)
    val (repr, reprLine) = place(byChurn(churnA) ++ byChurn(churnB) ++ facts.filter(_.packages(a)).map(_.source).sorted, used)
    val dep =
      if graph.sites.contains((a, b)) then s"`$a` depends on `$b`"
      else if graph.sites.contains((b, a)) then s"`$b` depends on `$a`"
      else "neither depends on the other"
    val less = if ea <= eb then a else b
    val sb = new StringBuilder
    sb.append(
      s"Packages `$a` and `$b` change together: $together of the ${math.min(ea, eb)} commits that touch `$less` also touch the other " +
        s"(${pct(ratio)}%; independent changes would overlap ${Slices.dec(lift)} times less). $summary.\n"
    )
    sb.append(
      "  Principle: things that change together belong together. A capability spread over two packages makes every change to it " +
        "a change in both, and each package's tests and reviewers see half of it.\n"
    )
    sb.append(s"  Relation: $dep.\n")
    if top.nonEmpty then
      sb.append("  Files changed together most:\n")
      for ((fa, fb), k) <- top do sb.append(f"    $k%3d commits  $fa  <->  $fb\n")
    sb.append("  Fix with one of:\n")
    sb.append(s"    - Move the files that change together into one package named after the capability they serve (a vertical slice), keeping the dependencies acyclic.\n")
    sb.append(s"    - If `$a` or `$b` exists only to serve the other, nest it below it (`$a.<name>`) or merge it.\n")
    sb.append("    - If one concept keeps forcing both to change, give it a package of its own that both depend on, so they can change apart.\n")
    sb.append(s"  Renaming files or splitting them does not help: the history follows renames, and the packages still change together.\n")
    sb.append(s"  Done when the files that change together are in one package; the finding clears once the window no longer shows them co-changing across packages.")
    Finding(s"co-change $a <-> $b", pct(ratio), repr, reprLine, sb.toString, List((1, Int.MaxValue)))

  // ---------------------------------------------------------------- feature spread

  private val camel = """[A-Z]+(?![a-z])|[A-Z]?[a-z]+""".r

  /** The business words in a name, normalised so that singular and plural, and a trailing vowel, meet. */
  private[plugin] def nouns(name: String, stop: Set[String]): List[(String, String)] =
    camel.findAllIn(name).toList.map(_.toLowerCase).filter(w => w.length >= 4 && !stop(w)).map(w => stem(w) -> w).distinct

  private[plugin] def stem(w: String): String =
    if w.length >= 6 && "aeio".contains(w.last) then w.init
    else if w.length >= 6 && w.endsWith("s") && !w.endsWith("ss") then w.init
    else w

  private def featureSpread(
      usable: List[Commit], facts: List[FileFacts], graph: PackageGraph, pkgs: FilePackages, o: Options, summary: String,
      used: mutable.Set[(String, Int)]
  ): (List[Finding], List[String]) =
    // file -> its words: from the file name and the top-level types it declares
    val words = facts.map { f =>
      val base = Path.of(f.source).getFileName.toString.stripSuffix(".scala")
      val names = base :: f.typeDecls.filter(d => !d.local && Slices.isTopLevel(d)).map(d => Slices.simple(d.sym))
      f.source -> names.flatMap(nouns(_, o.stopWords))
    }
    val byNoun = words.flatMap { case (file, ns) => ns.map { case (st, surface) => (st, surface, file) } }.groupBy(_._1)
    val pkgOf = facts.map(f => f.source -> f.packages.toList.sorted).toMap
    val related = Slices.families(graph.nodes)
    def families(ps: Set[String]) = ps.toList.sorted.foldLeft(List.empty[String])((acc, p) => if acc.exists(related(_, p)) then acc else p :: acc)

    val spread = byNoun.toList.sortBy(_._1).flatMap { case (st, entries) =>
      val files = entries.map(_._3).toSet
      val packages = files.flatMap(pkgOf(_)).filter(graph.nodes.contains)
      Option.when(families(packages).size >= o.minFeatureLayers) {
        val touching = usable.filter(c => c.files.exists(files))
        val across = touching.count(c => c.files.filter(files).flatMap(pkgs.of).size >= 2)
        val surface = entries.map(_._2).groupBy(identity).toList.sortBy(x => (-x._2.size, x._1.length, x._1)).head._1
        (st, surface, files, packages, touching.size, across)
      }
    }
    val candidates = spread.filter { case (_, _, _, _, touching, across) =>
      touching >= o.minCommits && across >= o.minCommits && across.toDouble / touching >= o.minCoChange
    }
    val unproven = spread.filter(_._5 < o.minCommits).sortBy(x => (-x._3.size, x._1)).map(_._2.capitalize)
    // `movimenti` and `registro movimenti` meet in the same files: report the one with more files
    val kept = mutable.ListBuffer[(String, String, Set[String], Set[String], Int, Int)]()
    for c <- candidates.sortBy(c => (-c._3.size, c._1)) do
      val dup = kept.exists(k => (k._3 intersect c._3).size.toDouble / (k._3 union c._3).size >= 0.6)
      if !dup then kept += c
    val found = kept.toList.sortBy(_._1).map { case (st, surface, files, packages, touching, across) =>
      spreadFinding(st, surface, files, packages, touching, across, facts, graph, summary, used)
    }
    (found, unproven)

  private def spreadFinding(
      st: String, surface: String, files: Set[String], packages: Set[String], touching: Int, across: Int,
      facts: List[FileFacts], graph: PackageGraph, summary: String, used: mutable.Set[(String, Int)]
  ): Finding =
    val byPackage = facts.filter(f => files(f.source)).flatMap(f => f.packages.filter(packages).map(_ -> f)).groupBy(_._1).toList.sortBy(_._1)
    val prefix = packages.toList.map(_.split('.').toList).reduce((x, y) => x.zip(y).takeWhile(p => p._1 == p._2).map(_._1)).mkString(".")
    val target = (if prefix.isEmpty then "" else prefix + ".") + surface.toLowerCase
    val plan = group(facts, files, packages, target)
    val sb = new StringBuilder
    sb.append(
      s"Feature `${surface.capitalize}` is spread over ${packages.size} packages: ${packages.toList.sorted.mkString(", ")}. " +
        s"$across of the $touching commits that touch its files touch more than one of those packages (${pct(across.toDouble / touching)}%). $summary.\n"
    )
    sb.append(
      "  Principle: cohesion by business capability. Packages by technical layer (model, database, rendering, routes) spread " +
        "one feature across all of them, so adding or changing it means editing each layer.\n"
    )
    sb.append("  Its files, by package:\n")
    for (p, fs) <- byPackage do
      val names = fs.map(x => Path.of(x._2.source).getFileName.toString).distinct.sorted
      sb.append(s"    $p: ${names.take(6).mkString(", ")}${if names.size > 6 then s", ... (${names.size})" else ""}\n")
    sb.append(s"  Proposed slice `$target` holding the ${plan.classes.size} top-level types declared in these files")
    if plan.extra.nonEmpty then sb.append(s" and the ${plan.extra.size} that only they use (${plan.extra.take(5).map(Slices.simple).mkString(", ")}${if plan.extra.size > 5 then ", ..." else ""})")
    sb.append(":\n")
    val shown = (plan.classes ++ plan.extra).sorted
    sb.append(s"    ${shown.take(10).map(Slices.simple).mkString(", ")}${if shown.size > 10 then s", ... (${shown.size})" else ""}\n")
    plan.cycle.foreach { others =>
      sb.append(s"  Check: moving them as they are creates a dependency cycle between `$target` and ${others.map(p => s"`$p`").mkString(", ")}; ")
      sb.append("move the classes that cycle with them, or invert the reference, before applying.\n")
    }
    sb.append("  Fix: package by capability: create the slice package, move these files into it (keep the layers inside it as files or subpackages), and let other slices depend on its public part only.\n")
    sb.append("  Renaming the files so that they no longer share the word does not help: the history that shows them changing together follows them.\n")
    sb.append(s"  Done when the files of `${surface.capitalize}` live in fewer than ${packages.size} packages.")
    val (repr, reprLine) = place(byPackage.flatMap(_._2.map(_._2.source)).distinct, used)
    Finding(s"feature-spread $st", packages.size, repr, reprLine, sb.toString, List((1, Int.MaxValue)))

  /** @param classes
    *   top-level types declared in the feature's files
    * @param extra
    *   types elsewhere that only those use, which would be orphaned by the move
    * @param cycle
    *   other packages that would form a cycle with the new one, if the move were made as is
    */
  private final case class Group(classes: List[String], extra: List[String], cycle: Option[List[String]])

  private def group(facts: List[FileFacts], files: Set[String], packages: Set[String], target: String): Group =
    val inFiles = facts.filter(f => files(f.source)).flatMap(f => f.typeDecls.filter(d => !d.local && Slices.isTopLevel(d)).map(Slices.topOf)).distinct
    val deps = facts.flatMap(_.classDeps)
    val users = deps.groupBy(_.toTop).view.mapValues(_.map(_.fromTop).distinct).toMap
    val members = mutable.LinkedHashSet.from(inFiles)
    // types only the feature uses go with it: they would stay behind as orphans of the old packages
    var grew = true
    while grew do
      grew = false
      for d <- deps.sortBy(_.toTop) if !members(d.toTop) && users.get(d.toTop).exists(us => us.nonEmpty && us.forall(members)) && d.toTop.contains('.') do
        if members.add(d.toTop) then grew = true
    val extra = members.toList.filterNot(inFiles.contains).sorted
    // the packages as they would be after the move
    val pkgOfTop = deps.flatMap(d => List(d.fromTop -> d.fromPkg, d.toTop -> d.toPkg)).toMap
    def assigned(top: String) = if members(top) then target else pkgOfTop.getOrElse(top, "")
    val sites = mutable.Map[(String, String), List[Edge]]()
    for d <- deps do
      val (f, t) = (assigned(d.fromTop), assigned(d.toTop))
      if f.nonEmpty && t.nonEmpty && f != t then
        sites((f, t)) = Edge(d.file, d.line, f, d.fromTop, t, d.toTop) :: sites.getOrElse((f, t), Nil)
    val nodes = (sites.keys.flatMap(k => List(k._1, k._2)).toSet + target).toList.sorted
    val regraph = new PackageGraph(nodes, sites.toMap)
    val cyc = regraph.components.find(_.contains(target)).filter(_.size > 1).map(_.filter(_ != target).sorted)
    Group(inFiles.sorted, extra, cyc)
