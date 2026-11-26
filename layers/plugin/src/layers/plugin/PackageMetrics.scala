package layers.plugin

import scala.collection.mutable

/** Robert Martin's package metrics and the Stable Dependencies Principle, computed on the package graph. */
object PackageMetrics:

  /** @param ca
    *   afferent coupling: packages that depend on this one
    * @param ce
    *   efferent coupling: packages this one depends on
    * @param abstractness
    *   abstract types / all types, None when the package declares no types
    * @param layer
    *   1 for packages depending on no other project package, else 1 + the highest layer among its dependencies
    */
  final case class Metric(pkg: String, ca: Int, ce: Int, abstractness: Option[Double], layer: Int):
    /** Ce / (Ca + Ce): 0 is maximally stable, 1 maximally unstable. */
    def instability: Double = if ca + ce == 0 then 0.0 else ce.toDouble / (ca + ce)

    /** Distance from the main sequence |A + I - 1|. */
    def distance: Option[Double] = abstractness.map(a => math.abs(a + instability - 1))

  /** `from` depends on `to`, which is less stable than `from`: changes in `to` ripple into the more stable `from`. */
  final case class SdpViolation(from: Metric, to: Metric, sites: List[Edge]):
    def key: String = s"sdp ${from.pkg} -> ${to.pkg}"

  private val tolerance = 1e-9

  def compute(graph: PackageGraph, facts: Iterable[FileFacts]): List[Metric] =
    val types = mutable.Map[String, (Int, Int)]()
    for f <- facts; (p, (total, abs)) <- f.types do
      val (t, a) = types.getOrElse(p, (0, 0))
      types(p) = (t + total, a + abs)

    val componentOf = graph.components.zipWithIndex.flatMap { case (c, i) => c.map(_ -> i) }.toMap
    val componentSucc = graph.components.indices.map { i =>
      i -> graph.components(i).flatMap(graph.succ.getOrElse(_, Nil)).map(componentOf).filter(_ != i).distinct
    }.toMap
    val layerOf = mutable.Map[Int, Int]()
    def layer(c: Int): Int =
      layerOf.getOrElseUpdate(c, 1 + componentSucc(c).map(layer).maxOption.getOrElse(0))

    graph.nodes.map { p =>
      val abstractness = types.get(p).filter(_._1 > 0).map { case (t, a) => a.toDouble / t }
      Metric(p, graph.afferent(p), graph.efferent(p), abstractness, layer(componentOf(p)))
    }

  /** Only meaningful on an acyclic graph; with cycles every edge of the cycle is half "wrong" by construction. */
  def sdpViolations(graph: PackageGraph, metrics: List[Metric]): List[SdpViolation] =
    val byPkg = metrics.map(m => m.pkg -> m).toMap
    graph.sites.toList.sortBy(_._1).collect {
      case ((f, t), sites) if byPkg(t).instability > byPkg(f).instability + tolerance =>
        SdpViolation(byPkg(f), byPkg(t), sites)
    }

  private val maxSitesShown = 5

  private def fmt(d: Double) = f"$d%.2f"

  def render(v: SdpViolation, graph: PackageGraph): String =
    val sb = new StringBuilder
    val (a, b) = (v.from, v.to)
    sb.append(s"Stable Dependencies violation: ${a.pkg} -> ${b.pkg}\n")
    sb.append(
      s"  Principle: depend in the direction of stability. `${a.pkg}` is more stable (instability ${fmt(a.instability)}: ${a.ca} dependents, ${a.ce} dependencies) " +
        s"than `${b.pkg}` (instability ${fmt(b.instability)}: ${b.ca} dependents, ${b.ce} dependencies), so every change in `${b.pkg}` ripples into `${a.pkg}` and its dependents.\n"
    )
    val n = v.sites.size
    sb.append(s"  References ($n):\n")
    for s <- v.sites.take(maxSitesShown) do sb.append(s"    ${s.file}:${s.line}  ${s.fromSym} -> ${s.toSym}\n")
    if n > maxSitesShown then sb.append(s"    ... and ${n - maxSitesShown} more\n")
    sb.append("  Fix with one of:\n")
    sb.append(s"    - Dependency inversion: declare a trait in `${a.pkg}` (or a stable third package) for what `${a.pkg}` needs from `${b.pkg}`; implement it in `${b.pkg}`.\n")
    sb.append(s"    - Move the referenced `${b.pkg}` symbols into `${a.pkg}` or into a package at least as stable.\n")
    val bDeps = graph.succ.getOrElse(b.pkg, Nil)
    if bDeps.nonEmpty then
      sb.append(s"    - Make `${b.pkg}` more stable by removing its own dependencies: ${bDeps.mkString(", ")}.\n")
    sb.append(s"  Done when recompiling no longer reports this edge (or `${b.pkg}` is no less stable than `${a.pkg}`).")
    sb.toString

  def finding(v: SdpViolation, graph: PackageGraph): Finding =
    val site = v.sites.head
    Finding(v.key, 0, site.file, site.line, render(v, graph))

  /** Metrics table, most unstable layer first. */
  def table(metrics: List[Metric]): String =
    val rows = metrics.sortBy(m => (-m.layer, m.pkg)).map { m =>
      f"${m.pkg}%-40s ${m.layer}%5d ${m.ca}%4d ${m.ce}%4d ${fmt(m.instability)}%6s ${m.abstractness.map(fmt).getOrElse("-")}%6s ${m.distance.map(fmt).getOrElse("-")}%6s"
    }
    (f"${"package"}%-40s ${"layer"}%5s ${"Ca"}%4s ${"Ce"}%4s ${"I"}%6s ${"A"}%6s ${"D"}%6s" :: rows).mkString("\n")
