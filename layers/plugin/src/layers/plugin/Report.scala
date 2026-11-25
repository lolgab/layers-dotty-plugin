package layers.plugin

import java.nio.file.{Files, Path}

/** `-P:layers:report=<file>`: every measured value, not only the ones over a limit, so it is visible how close the
  * code is to each limit and where a limit would bite next.
  *
  * One tab-separated line per measurement, sorted: `kind`, `symbol`, `value`, `limit`, `location`, `detail`. Kinds for
  * methods (a `val` initialiser is `symbol`, class-body statements are `<Class>.<init>`): `cyclomatic`, `cognitive`,
  * `lines`, `nesting` (each measured together with the single-use helpers absorbed into the method; `detail` gives
  * the method's own value and what each helper adds, `flat` marks a method long enough for the flat-layout limit, `cluster` one whose pieces are each within the limits and together within the cluster caps) and `params`. Kinds for classes and objects: `responsibilities`, `methods`,
  * `exports`, `fanin` (packages using it); for files: `filelines`. Architecture: `layer-mix` (layers a method touches,
  * methods of at least two), `slice-fanout`, `slice-fanin`, `slice-instability` (percent) and `slice-types` and `slice-owned-share` (percent of types owned by one slice or unused) per slice,
  * `god-fields` and `god-packages` per domain class with many fields. Every other check (domain purity, co-change, ...)
  * contributes a `finding` row per finding. A limit of `-` means there is none.
  */
object Report:

  private def where(file: String, line: Int) = s"${Path.of(file).getFileName}:$line"

  private type Row = (String, String, Int, String, String, String)

  /** Values of the architecture and slice checks, and a row for each finding of the checks that have no measure of
    * their own, so the file lists everything the build is judged on.
    */
  def architecture(all: List[FileFacts], graph: PackageGraph, cfg: ArchConfig, options: List[String], findings: List[Finding]): List[Row] =
    val rows = List.newBuilder[Row]
    val domain = Domains.infer(cfg, all, graph)
    for mix <- LayerMix.measure(all, domain, cfg) if mix.used.size >= 2 do
      val m = mix.effective.fact
      val layers = mix.used.map(_._1).mkString(",")
      rows += (("layer-mix", DomainPurity.clean(m.sym), mix.used.size, cfg.maxLayersPerMethod.toString, where(m.file, m.line), s"cyclomatic=${m.cyclomatic} $layers"))
    val so = SliceOptions.fromOptions(options)
    val slices = new Slices(graph, all, so)
    for p <- slices.slices do
      val deps = slices.dependencies(p)
      val users = slices.dependents(p)
      rows += (("slice-fanout", p, deps.size, so.maxFanOut.toString, p, ""))
      rows += (("slice-fanin", p, users.size, so.maxFanIn.toString, p, ""))
      if users.nonEmpty then
        rows += (("slice-instability", p, math.round(slices.instability(p) * 100).toInt, math.round(so.maxHubInstability * 100).toString, p, "percent"))
        val types = slices.topLevelTypes(p)
        rows += (("slice-types", p, types.map(Slices.topOf).distinct.size, so.maxHubTypes.toString, p, ""))
        val own = SliceCoupling.hubOwnership(slices, p, types)
        rows += (("slice-owned-share", p, own.percent, so.hubOwnedShare.toString, p, s"owned=${own.owned.values.map(_.size).sum} unused=${own.unused} blocked=${own.blocked} of ${own.total} percent"))
    val users = all.flatMap(_.classDeps).groupBy(_.toTop).view.mapValues(_.map(_.fromPkg).toSet).toMap
    for r <- ArchRows.of(all, "classinfo") if domain(r.field(1)) do
      val sym = r.field(0)
      rows += (("god-fields", sym, r.field(3).toInt, cfg.godTypeMinFields.toString, where(r.file, r.line), ""))
      rows += (("god-packages", sym, (users.getOrElse(sym, Set.empty) - r.field(1)).size, cfg.godTypeMinPackages.toString, where(r.file, r.line), ""))
    val measured = Set(
      "cyclomatic", "cognitive", "lines", "nesting", "params", "cohesion", "facade", "filesize", "layer-mix", "slice-fanout", "god-type"
    )
    for f <- findings do
      val (kind, sym) = f.key.span(_ != ' ')
      if !measured(kind) then rows += ((kind, sym.trim, f.value, "-", where(f.file, f.line), "finding"))
    rows.result()

  def lines(all: List[FileFacts], t: Thresholds, extra: List[Row] = Nil): List[String] =
    val methods = all.flatMap(_.methods)
    val fan = ClassMetrics.fanIn(all)
    val rows = List.newBuilder[(String, String, Int, String, String, String)]
    for e <- ClassMetrics.effective(methods, t) do
      val m = e.fact
      val loc = where(m.file, m.line)
      def detail(own: Int, of: MethodFact => Int, flat: Boolean = false) =
        val helpers = e.parts.sortBy(-of(_)).map(p => s"${p.sym.split('.').last}=${of(p)}").mkString(",")
        s"${if flat then "flat " else ""}own=$own" + (if e.parts.isEmpty then "" else s" merged-helpers=${e.parts.size}[$helpers]")
      rows += (("cyclomatic", m.sym, m.cyclomatic, t.maxCyclomatic.toString, loc, detail(e.own.cyclomatic, x => math.max(x.cyclomatic - 1, 0))))
      rows += (("cognitive", m.sym, m.cognitive, t.maxCognitive.toString, loc, detail(e.own.cognitive, _.cognitive)))
      rows += (("lines", m.sym, m.size, ClassMetrics.limitFor(m, t).toString, loc, (if e.cluster then "cluster " else "") + detail(e.own.size, _.size, ClassMetrics.limitFor(m, t) > t.maxMethodLines)))
      rows += (("nesting", m.sym, m.nesting, t.maxNesting.toString, loc, detail(e.own.nesting, _.nesting)))
    for m <- methods.groupBy(_.sym).values.map(_.maxBy(_.params)) do
      rows += (("params", m.sym, m.params, t.maxParams.toString, where(m.file, m.line), ""))
    for c <- all.flatMap(_.classes).groupBy(_.sym).values.map(_.head) do
      val loc = where(c.file, c.line)
      val grab = ClassMetrics.grabBag(c, fan)
      ClassMetrics.responsibilityCount(c, grab).foreach { n =>
        rows += (("responsibilities", c.sym, n, t.maxResponsibilities.toString, loc, if grab then "grab-bag" else ""))
      }
      rows += (("methods", c.sym, c.methods.size, "-", loc, ""))
      rows += (("exports", c.sym, c.exports, t.maxExports.toString, loc, ""))
      if fan.contains(c.sym) then rows += (("fanin", c.sym, fan(c.sym), "-", loc, ""))
    rows ++= extra
    for f <- all do rows += (("filelines", Path.of(f.source).getFileName.toString, f.lines, t.maxFileLines.toString, f.source, ""))
    "# kind\tsymbol\tvalue\tlimit\tlocation\tdetail" ::
      rows.result().sortBy(r => (r._1, r._2, r._5)).map { case (k, s, v, l, loc, d) => List(k, s, v.toString, l, loc, d).mkString("\t") }

  def write(file: Path, all: List[FileFacts], t: Thresholds, extra: List[Row] = Nil): Unit =
    Option(file.toAbsolutePath.getParent).foreach(Files.createDirectories(_))
    Files.writeString(file, lines(all, t, extra).mkString("", "\n", "\n"))
