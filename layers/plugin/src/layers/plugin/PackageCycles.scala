package layers.plugin

import scala.collection.mutable

/** Finds cyclic dependencies between packages and computes the cheapest set of package edges whose removal makes the
  * cycle acyclic, with every reference site that has to change.
  */
object PackageCycles:

  /** A package edge `from -> to` that must go, with every reference behind it. */
  final case class CutEdge(from: String, to: String, sites: List[Edge])

  /** One strongly connected component of packages.
    *
    * @param packages
    *   members, sorted
    * @param cut
    *   package edges to remove (minimum number of references to change), sorted by cost then name
    * @param order
    *   members from "uses the others" to "used by the others" once `cut` is applied
    * @param edgeCount
    *   number of package edges inside the component
    */
  final case class Cycle(packages: List[String], cut: List[CutEdge], order: List[String], edgeCount: Int)

  /** Exact search up to this many packages in one cycle (2^n * n), greedy ordering plus local search beyond. */
  private val exactLimit = 20

  /** A package may depend on its own subpackages (the same rule the annotations apply), so cutting such an edge is
    * almost never the right fix: prefer cutting the subpackage's dependency on its parent.
    */
  private val downwardPenalty = 1000

  /** Cost of removing the edge `from -> to` made of `lines` references. */
  private[plugin] def edgeWeight(from: String, to: String, lines: Int): Int =
    lines * (if to.startsWith(from + ".") then downwardPenalty else 1)

  def analyze(facts: Iterable[FileFacts]): List[Cycle] = analyze(PackageGraph.build(facts))

  def analyze(graph: PackageGraph): List[Cycle] =
    graph.components.filter(_.size > 1).map { scc =>
      val members = scc.sorted
      val inside = members.toSet
      // A reference is a place to change: count source lines, not symbol pairs (macros expand one line into many).
      val weights = graph.sites.collect {
        case ((f, t), es) if inside(f) && inside(t) =>
          (f, t) -> edgeWeight(f, t, es.map(e => (e.file, e.line)).distinct.size)
      }
      val order = linearOrder(members, weights)
      val pos = order.zipWithIndex.toMap
      val cut = weights.keys.filter { case (f, t) => pos(f) > pos(t) }.toList
        .map { case (f, t) => CutEdge(f, t, graph.sites((f, t)).distinctBy(e => (e.file, e.line))) }
        .sortBy(c => (c.sites.size, c.from, c.to))
      Cycle(members, cut, order, weights.size)
    }.sortBy(_.packages.head)

  /** Order `members` so the total weight of edges pointing backwards (from a later to an earlier member) is minimal:
    * the minimum weighted feedback arc set. Exact for small components, Eades-Lin-Smyth heuristic otherwise.
    */
  private[plugin] def linearOrder(members: List[String], weights: Map[(String, String), Int]): List[String] =
    if members.size <= exactLimit then exactOrder(members, weights) else greedyOrder(members, weights)

  private def exactOrder(members: List[String], weights: Map[(String, String), Int]): List[String] =
    val n = members.size
    val idx = members.zipWithIndex.toMap
    // back(v)(u): weight of v -> u, paid when v is placed after u
    val toward = Array.ofDim[Int](n, n)
    for ((f, t), w) <- weights do toward(idx(f))(idx(t)) = w
    val full = (1 << n) - 1
    val cost = Array.fill(full + 1)(Int.MaxValue)
    val last = Array.fill(full + 1)(-1)
    cost(0) = 0
    for mask <- 0 until full; if cost(mask) != Int.MaxValue; v <- 0 until n if (mask & (1 << v)) == 0 do
      var add = 0
      var u = 0
      while u < n do
        if (mask & (1 << u)) != 0 then add += toward(v)(u)
        u += 1
      val next = mask | (1 << v)
      if cost(mask) + add < cost(next) then
        cost(next) = cost(mask) + add
        last(next) = v
    val order = mutable.ListBuffer[String]()
    var mask = full
    while mask != 0 do
      val v = last(mask)
      order.prepend(members(v))
      mask &= ~(1 << v)
    order.toList

  private[plugin] def greedyOrder(members: List[String], weights: Map[(String, String), Int]): List[String] =
    val remaining = mutable.LinkedHashSet.from(members)
    def out(v: String) = weights.iterator.collect { case ((f, t), w) if f == v && remaining(t) => w }.sum
    def in(v: String) = weights.iterator.collect { case ((f, t), w) if t == v && remaining(f) => w }.sum
    val front = mutable.ListBuffer[String]()
    val back = mutable.ListBuffer[String]()
    while remaining.nonEmpty do
      remaining.find(out(_) == 0) match
        case Some(sink) =>
          back.prepend(sink)
          remaining -= sink
        case None =>
          remaining.find(in(_) == 0) match
            case Some(source) =>
              front += source
              remaining -= source
            case None =>
              val best = remaining.maxBy(v => out(v) - in(v))
              front += best
              remaining -= best
    improve(front.toList ++ back.toList, weights)

  private[plugin] def backwardCost(order: List[String], weights: Map[(String, String), Int]): Int =
    val pos = order.zipWithIndex.toMap
    weights.iterator.collect { case ((f, t), w) if pos(f) > pos(t) => w }.sum

  /** Moves single packages to the position that lowers the cost most, until no move helps. */
  private def improve(start: List[String], weights: Map[(String, String), Int]): List[String] =
    var order = start
    var best = backwardCost(order, weights)
    var improved = true
    while improved do
      improved = false
      for v <- start do
        val without = order.filterNot(_ == v)
        val candidates = (0 to without.size).map { i =>
          val (a, b) = without.splitAt(i)
          a ++ (v :: b)
        }
        val (bestOrder, bestCost) = candidates.map(o => (o, backwardCost(o, weights))).minBy(_._2)
        if bestCost < best then
          order = bestOrder
          best = bestCost
          improved = true
    order

  private val maxSitesShown = 5

  /** The compiler error text for one cycle. */
  def render(c: Cycle, plan: Option[MovePlan.Plan] = None): String =
    val sb = new StringBuilder
    sb.append(s"Cyclic dependency between packages: ${c.packages.mkString(", ")}\n")
    sb.append("  Principle: Acyclic Dependencies Principle. Packages in a cycle cannot be understood, tested or changed independently.\n")
    sb.append(
      s"  Remove these ${c.cut.size} of ${c.edgeCount} package edges (fewest references to change) to make the cycle acyclic:\n"
    )
    for cut <- c.cut do
      val n = cut.sites.size
      sb.append(s"    ${cut.from} -> ${cut.to} ($n reference${if n == 1 then "" else "s"}):\n")
      for s <- cut.sites.take(maxSitesShown) do sb.append(s"      ${s.file}:${s.line}  ${s.fromSym} -> ${s.toSym}\n")
      if n > maxSitesShown then sb.append(s"      ... and ${n - maxSitesShown} more\n")
    sb.append("  Fix each edge `A -> B` with one of:\n")
    sb.append("    - Dependency inversion: declare a trait in A for what A needs from B and implement it in B.\n")
    sb.append("    - Move the referenced B symbols into A, or the referencing A code into B.\n")
    plan.foreach(p => sb.append(MovePlan.render(p)))
    sb.append(s"  Dependency order once fixed (each package may use the ones after it): ${c.order.mkString(" > ")}\n")
    sb.append("  Done when recompiling no longer reports a cycle among these packages.")
    sb.toString
