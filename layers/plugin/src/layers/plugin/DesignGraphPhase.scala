package layers.plugin

import java.nio.file.{Files, Path}

import scala.collection.mutable

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.YCheckPositions
import dotty.tools.dotc.util.{NoSourcePosition, SourcePosition, SrcPos}
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.io.AbstractFile

import SymbolNames.{isStdLib, packageOf, topLevelName}

/** Design checks inferred from the code, on top of the annotation checks. Enabled with `-P:layers:design`.
  *
  * Records the cross-package references of every compiled file next to the class output, merges them with the
  * records of files this run did not recompile, and fails compilation on cyclic package dependencies and on
  * Stable Dependencies Principle violations.
  *
  * Options: `baseline=<file>` accepted legacy violations, `updateBaseline` to create or tighten it, `metrics` to print
  * the package metrics table.
  */
class DesignGraphPhase(options: List[String] = Nil)(using Context) extends PluginPhase:
  import tpd.*

  val phaseName = "layers-design"

  override val runsAfter = Set(YCheckPositions.name)

  // Cycles are the more fundamental problem: report them before the annotation checks, which a cycle makes noisy.
  override val runsBefore = Set("layers")

  private var lastRun: AnyRef | Null = null

  override def run(using ctx: Context): Unit =
    // Analysis spans all units, so do it once per compiler run however often the phase is invoked.
    if ctx.run ne lastRun then
      lastRun = ctx.run
      usedOffsets.clear()
      analyzeRun()
    super.run

  private def analyzeRun()(using ctx: Context): Unit =
    val units = ctx.run.nn.units.filter(u => u.source != null && u.source.exists && !u.tpdTree.isEmpty)
    val fresh = units.map(extract)
    val store = factsStore
    store.foreach(s => fresh.foreach(s.write))
    val loaded = store.map(_.loadOthers(fresh.map(_.source).toSet)).getOrElse(FactsStore.Loaded(Nil, Nil))
    if loaded.outdated.nonEmpty then
      report.error(
        s"[layers] The design facts of ${loaded.outdated.size} unchanged files were written by another plugin version " +
          "and cannot be reused, so the design checks would only see part of the project.\n" +
          "  Fix: recompile the whole module (clean its compile output, e.g. `mill clean <module>.compile`).\n" +
          "  Files: " + loaded.outdated.sorted.take(5).mkString(", ") + (if loaded.outdated.size > 5 then ", ..." else ""),
        NoSourcePosition
      )
    val all = fresh ++ loaded.facts
    val archConfig = ArchConfig.fromOptions(options)
    val staleArch = ArchChecks.stale(all, archConfig)
    if staleArch.nonEmpty then
      report.error(
        s"[layers] The architecture facts of ${staleArch.size} files were extracted with other identityTypes/layerKinds options.\n" +
          "  Fix: recompile the whole module (clean its compile output, e.g. `mill clean <module>.compile`).\n" +
          "  Files: " + staleArch.take(5).mkString(", ") + (if staleArch.size > 5 then ", ..." else ""),
        NoSourcePosition
      )
    val graph = PackageGraph.build(all)
    val metrics = PackageMetrics.compute(graph, all)
    if options.contains("metrics") then report.echo(PackageMetrics.table(metrics))

    val cycles = PackageCycles.analyze(graph)
    for cycle <- cycles do
      val site = cycle.cut.headOption.flatMap(_.sites.headOption)
      val plan = MovePlan.suggest(graph, cycle, all)
      report.error(s"[layers] ${PackageCycles.render(cycle, plan)}", positionOfSite(site))
    val stability =
      // With cycles, instability is not meaningful: fix those first.
      if cycles.isEmpty then PackageMetrics.sdpViolations(graph, metrics).map(PackageMetrics.finding(_, graph)) else Nil
    val thresholds = Thresholds.fromOptions(options)
    val members = ClassMetrics.findings(all.flatMap(_.methods), all.flatMap(_.classes), thresholds, ClassMetrics.fanIn(all))
    if options.contains("hotspots") then
      all.headOption.foreach(f => reportHistory(all, Path.of(f.source)))
    val scatter = ChangeScatter.run(options, all, graph)
    scatter.warnings.foreach(report.warning(_, NoSourcePosition))
    scatter.note.foreach(report.echo(_))
    val allFindings =
      stability ++ members ++ Slop.findings(all, thresholds) ++ ClassMetrics.fileFindings(all, thresholds) ++
        ArchChecks.findings(all, graph, archConfig) ++ SliceChecks.findings(all, graph, options) ++ scatter.findings
    options.collectFirst { case o if o.startsWith("report=") => Path.of(o.drop(7)) }
      .foreach(Report.write(_, all, thresholds, Report.architecture(all, graph, archConfig, options, allFindings)))
    checkBaseline(
      allFindings,
      stabilityChecked = cycles.isEmpty,
      anyFile = all.headOption.map(f => Path.of(f.source)),
      historyChecked = scatter.checked
    )

  /** Files that change often and are complex, and files that change together across packages. */
  private def reportHistory(all: List[FileFacts], anyFile: Path)(using Context): Unit =
    val window = options.collectFirst { case o if o.startsWith("history=") => o.drop(8) }.getOrElse("1 year ago")
    GitSignals.history(window, anyFile) match
      case None => report.warning("[layers] hotspots: git history is not available", NoSourcePosition)
      case Some((root, commits)) =>
        def abs(f: String) = root.resolve(f).toAbsolutePath.normalize.toString
        val absCommits = commits.map(_.map(abs))
        val churn = GitSignals.churn(absCommits)
        val methods = all.flatMap(f => f.methods.map(f.source -> _))
        val complexity = methods.groupBy(_._1).view.mapValues(ms => ClassMetrics.effective(ms.map(_._2)).map(_.fact.cognitive).sum).toMap
        val hot = GitSignals.hotspots(churn, complexity)
        val pkgOf = all.flatMap(f => f.packages.headOption.map(f.source -> _)).toMap
        val coupled = GitSignals.coChange(absCommits).filter(c => pkgOf.contains(c.a) && pkgOf.contains(c.b) && pkgOf(c.a) != pkgOf(c.b)).take(10)
        val name = (f: String) => root.relativize(Path.of(f)).toString
        val sb = new StringBuilder
        sb.append(s"[layers] hotspots (commits since $window x cognitive complexity): where cleaning pays back first\n")
        for h <- hot do sb.append(f"  ${h.score}%7d  ${h.commits}%4d commits  ${h.complexity}%5d complexity  ${name(h.file)}\n")
        if coupled.nonEmpty then
          sb.append(s"[layers] files in different packages that change together (hidden coupling)\n")
          for c <- coupled do
            sb.append(f"  ${c.together}%4d commits  ${(c.ratio * 100).toInt}%3d%%  ${name(c.a)}  <->  ${name(c.b)}\n")
        report.echo(sb.toString)

  private def checkBaseline(allFindings: List[Finding], stabilityChecked: Boolean, anyFile: Option[Path], historyChecked: Boolean = true)(using Context): Unit =
    val since = options.collectFirst { case o if o.startsWith("since=") => o.drop(6) }
    val changes = for rev <- since; f <- anyFile yield GitSignals.changesSince(rev, f)
    if since.isDefined && changes.exists(_.isEmpty) then
      report.warning(s"[layers] since=${since.get} ignored: git cannot compare against it here, so everything is enforced", NoSourcePosition)
    val (findings, existing) = changes.flatten match
      case Some(c) => Ratchet.split(allFindings, c)
      case None    => (allFindings, Nil)
    if existing.nonEmpty then report.warning(Ratchet.summary(existing, since.get), NoSourcePosition)
    val ratcheting = changes.flatten.isDefined
    val baselineFile = options.collectFirst { case o if o.startsWith("baseline=") => Path.of(o.drop(9)) }
    val baseline = baselineFile.filter(Files.exists(_)).map(f => Baseline.parse(Files.readString(f)))
    val outcome = Baseline.evaluate(
      findings,
      baseline,
      update = options.contains("updateBaseline") && baselineFile.isDefined && !ratcheting,
      unchecked = key => (!stabilityChecked && key.startsWith("sdp ")) || (!historyChecked && ChangeScatter.isHistoryKey(key))
    )
    for (file, entries) <- baselineFile.zip(outcome.write) do
      Option(file.toAbsolutePath.getParent).foreach(Files.createDirectories(_))
      Files.writeString(file, Baseline.render(entries))
    // The compiler shows one error per source position: findings of one declaration (cyclomatic, cognitive, lines...)
    // each get their own offset within the line, or only the first would be visible.
    val seen = mutable.Map[(String, Int), Int]().withDefaultValue(0)
    for f <- outcome.failing do
      val shift = seen((f.file, f.line))
      seen((f.file, f.line)) = shift + 1
      report.error(s"[layers] ${f.message}", positionOf(f.file, f.line, shift))
    if outcome.stale.nonEmpty && !ratcheting then
      report.error(
        s"[layers] Baseline ${baselineFile.get} is out of date: these findings improved or were fixed. Lock in the " +
          "improvement by recompiling once with -P:layers:updateBaseline, which only lowers or removes entries.\n  " +
          outcome.stale.mkString("\n  "),
        NoSourcePosition
      )

  private def positionOfSite(site: Option[Edge])(using Context): SrcPos =
    site.map(s => positionOf(s.file, s.line)).getOrElse(NoSourcePosition)

  /** Facts live in the class output directory; nothing is persisted when the output is not a directory. */
  private def factsStore(using ctx: Context): Option[FactsStore] =
    val out = ctx.settings.outputDir.value
    Option(out.file).filter(_.isDirectory).map(f => new FactsStore(f.toPath.resolve("META-INF").resolve("layers-facts")))

  /** The compiler hides an error at an offset where another error was already reported, comparing offsets only and not
    * files. Findings of one method (`shift`) and findings at the start of different files would hide each other, so
    * each one gets its own column.
    */
  private val usedOffsets = mutable.Set[Int]()

  private def positionOf(file: String, line: Int, shift: Int = 0)(using ctx: Context): SrcPos =
    try
      val source = ctx.getSource(AbstractFile.getFile(Path.of(file)))
      val start = source.lineToOffset(line - 1)
      val nl = source.content.indexOf('\n', start)
      val end = if nl < 0 then source.content.length else nl
      var offset = math.max(start, math.min(start + shift, end))
      while !usedOffsets.add(offset) && offset < source.content.length - 1 do offset += 1
      SourcePosition(source, Span(offset))
    catch case _: Exception => NoSourcePosition

  private def extract(unit: CompilationUnit)(using Context): FileFacts =
    val source = Path.of(unit.source.file.path).toAbsolutePath.normalize.toString
    val packages = mutable.Set[String]()
    val edges = mutable.LinkedHashMap[(String, String), Edge]()
    val types = mutable.Map[String, (Int, Int)]()
    val methods = mutable.ListBuffer[MethodFact]()
    val classes = mutable.ListBuffer[ClassFact]()
    val passThroughs = mutable.ListBuffer[PassThroughFact]()
    val clones = mutable.ListBuffer[CloneFact]()
    val typeDecls = mutable.ListBuffer[TypeDecl]()
    val bundles = mutable.ListBuffer[BundleDecl]()
    val bundleUses = mutable.ListBuffer[BundleUse]()
    val classDeps = mutable.LinkedHashMap[(String, String), (String, String, mutable.LinkedHashSet[Int])]()
    val tops = mutable.Map[Symbol, String]()
    val arch0 = ArchExtract.extract(unit, source, ArchConfig.fromOptions(options))
    val entries = mutable.ListBuffer[ArchFact]()

    /** `@main`, `def main(args: Array[String])` and `extends App`: where the program starts. */
    def entryPoint(sym: Symbol): Boolean =
      sym.exists && (
        (sym.is(Flags.Method) && sym.owner.isClass &&
          (sym.hasAnnotation(defn.MainAnnot) ||
            (sym.name.toString == "main" && sym.info.paramInfoss.flatten.exists(_.typeSymbol == defn.ArrayClass)))) ||
        (sym.isClass && !sym.is(Flags.Trait) && sym.info.baseClasses.exists(_.fullName.toString == "scala.App"))
      )

    def countType(sym: Symbol): Unit =
      if sym.isClass && !sym.is(Flags.ModuleClass) && !sym.is(Flags.Synthetic) && !sym.isAnonymousClass then
        val pkg = packageOf(sym)
        if pkg != "root" then
          val isAbstract = sym.is(Flags.Trait) || sym.is(Flags.Abstract)
          val (total, abs) = types.getOrElse(pkg, (0, 0))
          types(pkg) = (total + 1, abs + (if isAbstract then 1 else 0))

    def top(sym: Symbol): String = tops.getOrElseUpdate(sym, topLevelName(sym))

    def record(from: Symbol, to: Symbol, span: Span): Unit =
      if from.exists && to.exists && !to.is(Flags.Package) then
        val fromPkg = packageOf(from)
        val toPkg = packageOf(to)
        val (fromTop, toTop) = (top(from), top(to))
        if fromTop.nonEmpty && toTop.nonEmpty && fromTop != toTop && fromPkg != "root" && toPkg != "root" && !isStdLib(toPkg)
        then
          val at = if span.exists then span else from.span
          val line = if at.exists then unit.source.offsetToLine(at.point) + 1 else 0
          classDeps.getOrElseUpdate((fromTop, toTop), (fromPkg, toPkg, mutable.LinkedHashSet()))._3 += line
        if fromPkg != toPkg && fromPkg != "root" && toPkg != "root" && !isStdLib(toPkg) then
          val effective = if span.exists then span else from.span
          val line = if effective.exists then unit.source.offsetToLine(effective.point) + 1 else 0
          val key = (from.fullName.toString, to.fullName.toString)
          val edge = Edge(source, line, fromPkg, key._1, toPkg, key._2)
          edges.get(key) match
            case Some(old) if old.line <= line => ()
            case _                             => edges(key) = edge

    val traverser = new TreeTraverser:
      private var current: Symbol = NoSymbol

      def traverse(tree: Tree)(using Context): Unit = tree match
        case _: Import => ()
        case pd: PackageDef =>
          if pd.stats.exists { case _: PackageDef | _: Import => false; case _ => true } then
            val pkg = packageOf(pd.symbol)
            if pkg != "root" then packages += pkg
          pd.stats.foreach(traverse)
        case d: (DefDef | ValDef | TypeDef)
            if d.symbol.exists && (!d.symbol.is(Flags.Synthetic) || d.symbol.name.toString.contains("$package")) =>
          if !d.isInstanceOf[ValDef] && entryPoint(d.symbol) then
            val at = if d.span.exists then unit.source.offsetToLine(d.span.point) + 1 else 0
            entries += ArchFact("entry", source, at, List(packageOf(d.symbol)))
          val saved = current
          d match
            case td: TypeDef =>
              countType(td.symbol)
              MemberMetrics.cls(td, unit).foreach(c => classes += c.copy(file = source))
              MemberMetrics.initializer(td, unit).foreach(m => methods += m.copy(file = source))
              SlopMetrics.typeDecl(td, unit).foreach(x => typeDecls += x.copy(file = source))
              SlopMetrics.bundleDecl(td, unit).foreach(x => bundles += x.copy(file = source))
            case dd: DefDef =>
              MemberMetrics.method(dd, unit).foreach(m => methods += m.copy(file = source))
              SlopMetrics.passThrough(dd, unit).foreach(x => passThroughs += x.copy(file = source))
              SlopMetrics.cloneFact(dd, unit).foreach(x => clones += x.copy(file = source))
              bundleUses ++= SlopMetrics.bundleParams(dd, unit).map(x => x.copy(file = source))
            case vd: ValDef =>
              MemberMetrics.valMember(vd, unit).foreach(m => methods += m.copy(file = source))
            case _ =>
          // Macro-generated helpers (`_$x`) are reported as the method that contains them.
          if !d.symbol.name.toString.startsWith("_$") then current = d.symbol
          traverseChildren(d)
          current = saved
        case t @ Apply(_, _) if SlopMetrics.constructed(t).isDefined =>
          val line = if t.span.exists then unit.source.offsetToLine(t.span.point) + 1 else 0
          bundleUses += BundleUse(source, line, "new", SlopMetrics.constructed(t).get, current.fullName.toString)
          traverseChildren(t)
        case t: TypeTree =>
          for sym <- SymbolNames.classesIn(t.tpe) do
            record(current, sym, t.span)
            if SlopMetrics.bundleCandidate(sym) then
              val line = if t.span.exists then unit.source.offsetToLine(t.span.point) + 1 else 0
              bundleUses += BundleUse(source, line, "ref", sym.fullName.toString, current.fullName.toString)
        case t: (Ident | Select) =>
          record(current, t.symbol, t.span)
          traverseChildren(t)
        case _ =>
          traverseChildren(tree)

    traverser.traverse(unit.tpdTree)
    FileFacts(source, packages.toSet, edges.values.toList, types.toMap, methods.toList, classes.toList,
      passThroughs.toList, clones.toList, typeDecls.toList,
      classDeps.toList.map { case ((f, t), (fp, tp, lines)) => ClassDep(source, lines.min, f, t, fp, tp, lines.size) },
      bundles.toList, bundleUses.toList, unit.source.content.count(_ == '\n') + 1, arch0 ++ entries)
