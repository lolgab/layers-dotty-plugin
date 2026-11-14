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
    val members = ClassMetrics.findings(all.flatMap(_.methods), all.flatMap(_.classes), thresholds)
    checkBaseline(stability ++ members ++ Slop.findings(all, thresholds), stabilityChecked = cycles.isEmpty)

  private def checkBaseline(findings: List[Finding], stabilityChecked: Boolean)(using Context): Unit =
    val baselineFile = options.collectFirst { case o if o.startsWith("baseline=") => Path.of(o.drop(9)) }
    val baseline = baselineFile.filter(Files.exists(_)).map(f => Baseline.parse(Files.readString(f)))
    val outcome = Baseline.evaluate(
      findings,
      baseline,
      update = options.contains("updateBaseline") && baselineFile.isDefined,
      unchecked = key => !stabilityChecked && key.startsWith("sdp ")
    )
    for (file, entries) <- baselineFile.zip(outcome.write) do
      Option(file.toAbsolutePath.getParent).foreach(Files.createDirectories(_))
      Files.writeString(file, Baseline.render(entries))
    for f <- outcome.failing do
      report.error(s"[layers] ${f.message}", positionOf(f.file, f.line))
    if outcome.stale.nonEmpty then
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

  private def positionOf(file: String, line: Int)(using ctx: Context): SrcPos =
    try
      val source = ctx.getSource(AbstractFile.getFile(Path.of(file)))
      SourcePosition(source, Span(source.lineToOffset(line - 1)))
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
    val classDeps = mutable.LinkedHashMap[(String, String), (String, String, mutable.LinkedHashSet[Int])]()
    val tops = mutable.Map[Symbol, String]()

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
        case d: (DefDef | ValDef | TypeDef) if d.symbol.exists && !d.symbol.is(Flags.Synthetic) =>
          val saved = current
          d match
            case td: TypeDef =>
              countType(td.symbol)
              MemberMetrics.cls(td, unit).foreach(c => classes += c.copy(file = source))
              SlopMetrics.typeDecl(td, unit).foreach(x => typeDecls += x.copy(file = source))
            case dd: DefDef =>
              MemberMetrics.method(dd, unit).foreach(m => methods += m.copy(file = source))
              SlopMetrics.passThrough(dd, unit).foreach(x => passThroughs += x.copy(file = source))
              SlopMetrics.cloneFact(dd, unit).foreach(x => clones += x.copy(file = source))
            case _ =>
          // Macro-generated helpers (`_$x`) are reported as the method that contains them.
          if !d.symbol.name.toString.startsWith("_$") then current = d.symbol
          traverseChildren(d)
          current = saved
        case t: TypeTree =>
          for sym <- classesIn(t.tpe) do record(current, sym, t.span)
        case t: (Ident | Select) =>
          record(current, t.symbol, t.span)
          traverseChildren(t)
        case _ =>
          traverseChildren(tree)

    traverser.traverse(unit.tpdTree)
    FileFacts(source, packages.toSet, edges.values.toList, types.toMap, methods.toList, classes.toList,
      passThroughs.toList, clones.toList, typeDecls.toList,
      classDeps.toList.map { case ((f, t), (fp, tp, lines)) => ClassDep(source, lines.min, f, t, fp, tp, lines.size) })

  private def classesIn(tpe: Type)(using Context): Set[Symbol] =
    tpe match
      case ref: TypeRef =>
        val sym = ref.symbol
        if sym.exists && !sym.isPrimitiveValueClass then Set(sym) else Set.empty
      case app: AppliedType => classesIn(app.tycon) ++ app.args.flatMap(classesIn)
      case and: AndType     => classesIn(and.tp1) ++ classesIn(and.tp2)
      case or: OrType       => classesIn(or.tp1) ++ classesIn(or.tp2)
      case _                => Set.empty
