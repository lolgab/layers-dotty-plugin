package layers.plugin

import scala.collection.mutable

/** Proposes which classes to extract into new packages to break a package cycle.
  *
  * Works on top-level classes, the unit that moves. For each package edge that points backwards, the classes it
  * references, together with everything they in turn use inside the cycle, would form a package that uses nothing else
  * in the cycle. The move that lowers the remaining backward weight most is applied, and the search continues from the
  * result. This is evidence for a human or an agent, not a verdict: only a person can name the new package.
  */
object MovePlan:

  /** @param classes
    *   top-level classes to move together, sorted
    * @param files
    *   source file per class where known
    * @param from
    *   packages the classes are in now, with how many each
    * @param usedBy
    *   packages that use them
    * @param target
    *   placeholder name of the new package
    */
  final case class Move(
      classes: List[String],
      files: Map[String, String],
      from: List[(String, Int)],
      usedBy: List[String],
      target: String
  )

  /** @param costBefore
    *   weight of the references that point backwards now
    * @param costAfter
    *   the same after all moves; 0 means the cycle is gone
    */
  final case class Plan(moves: List[Move], costBefore: Int, costAfter: Int)

  private val maxMoves = 4

  /** A single move may carry at most this share of the classes in the cycle, else it is a rewrite, not an extraction. */
  private val maxShare = 0.4

  private final case class Dep(from: String, to: String, refs: Int)

  def suggest(graph: PackageGraph, cycle: PackageCycles.Cycle, facts: Iterable[FileFacts]): Option[Plan] =
    val inCycle = cycle.packages.toSet
    val all = facts.flatMap(_.classDeps).toList
    val assign = mutable.Map[String, String]()
    for d <- all do
      if inCycle(d.fromPkg) then assign(d.fromTop) = d.fromPkg
      if inCycle(d.toPkg) then assign(d.toTop) = d.toPkg
    val deps = all.filter(d => assign.contains(d.fromTop) && assign.contains(d.toTop))
      .groupBy(d => (d.fromTop, d.toTop)).toList
      .map { case ((f, t), ds) => Dep(f, t, ds.map(_.refs).sum) }
    val files = (facts.flatMap(_.typeDecls).map(t => t.sym -> t.file) ++ all.map(d => d.fromTop -> d.file)).toMap
    val maxMoved = math.max(1, (assign.size * maxShare).toInt)
    val usesOf = deps.groupBy(_.from).view.mapValues(_.map(_.to)).toMap
    // Classes already placed in a new package stay there; later moves only deal with what is left.
    val frozen = mutable.Set[String]()

    def evaluate(): (Int, Map[(String, String), Int], List[String]) =
      val weights = mutable.Map[(String, String), Int]()
      for d <- deps do
        val (pf, pt) = (assign(d.from), assign(d.to))
        if pf != pt then weights((pf, pt)) = weights.getOrElse((pf, pt), 0) + d.refs
      val priced = weights.toMap.map { case ((f, t), w) => (f, t) -> PackageCycles.edgeWeight(f, t, w) }
      val order = PackageCycles.greedyOrder(assign.values.toList.distinct.sorted, priced)
      (PackageCycles.backwardCost(order, priced), priced, order)

    def closure(seed: Set[String]): Set[String] =
      val seen = mutable.LinkedHashSet.from(seed)
      val queue = mutable.Queue.from(seed)
      while queue.nonEmpty do
        for t <- usesOf.getOrElse(queue.dequeue(), Nil) do
          if !frozen(t) && seen.add(t) then queue.enqueue(t)
      seen.toSet

    val (costBefore, _, _) = evaluate()
    var cost = costBefore
    val moves = List.newBuilder[Move]
    var count = 0
    var continue = cost > 0
    while continue && count < maxMoves do
      val (_, weights, _) = evaluate()
      // Any package edge may be the one to break, not only those pointing backwards in the current best order.
      val packageEdges = weights.keys.toList.sorted
      val candidates = packageEdges.flatMap { case (x, y) =>
        val referenced = deps.collect { case d if assign(d.from) == x && assign(d.to) == y && !frozen(d.to) => d.to }.toSet
        Some(closure(referenced)).filter(c => c.nonEmpty && c.size <= maxMoved)
      }.distinct
      val target = s"<new package ${count + 1}>"
      val scored = candidates.map { c =>
        val saved = c.toList.map(k => k -> assign(k))
        c.foreach(assign(_) = target)
        val (after, _, _) = evaluate()
        saved.foreach { case (k, p) => assign(k) = p }
        (c, after)
      }
      scored.minByOption { case (c, after) => (after, c.size, c.toList.sorted.head) } match
        case Some((c, after)) if after < cost =>
          val from = c.toList.groupBy(assign).view.mapValues(_.size).toList.sortBy(x => (-x._2, x._1))
          val usedBy = deps.collect { case d if c(d.to) && !c(d.from) => assign(d.from) }.distinct.sorted
          c.foreach { k =>
            assign(k) = target
            frozen += k
          }
          val sorted = c.toList.sorted
          moves += Move(sorted, sorted.flatMap(k => files.get(k).map(k -> _)).toMap, from, usedBy, target)
          cost = after
          count += 1
          continue = cost > 0
        case _ => continue = false

    val result = moves.result()
    Option.when(result.nonEmpty)(Plan(result, costBefore, cost))

  private val maxClassesShown = 12
  private val maxUsersShown = 8

  def render(plan: Plan): String =
    val sb = new StringBuilder
    sb.append("  Suggested restructuring, computed from class dependencies (judge it before applying, then recompile):\n")
    for (m, i) <- plan.moves.zipWithIndex do
      sb.append(s"    ${i + 1}. Extract these ${m.classes.size} classes into a new package. Name it for what they are:\n")
      for c <- m.classes.take(maxClassesShown) do
        sb.append(s"         $c${m.files.get(c).map(f => s"  ($f)").getOrElse("")}\n")
      if m.classes.size > maxClassesShown then sb.append(s"         ... and ${m.classes.size - maxClassesShown} more\n")
      val byFile = m.classes.flatMap(m.files.get).groupBy(identity).view.mapValues(_.size).toList.sortBy(x => (-x._2, x._1))
      if byFile.nonEmpty then
        sb.append(s"       Files: ${byFile.take(5).map { case (f, n) => s"${f.split('/').last} ($n)" }.mkString(", ")}${if byFile.size > 5 then ", ..." else ""}.\n")
      sb.append(s"       Now in: ${m.from.map { case (p, n) => s"$p ($n)" }.mkString(", ")}.\n")
      val users = m.usedBy.take(maxUsersShown).mkString(", ") + (if m.usedBy.size > maxUsersShown then s" and ${m.usedBy.size - maxUsersShown} more" else "")
      sb.append(s"       Used by: $users. They use nothing else in this cycle.\n")
    val verdict =
      if plan.costAfter == 0 then "the cycle is gone"
      else s"${plan.costAfter} of ${plan.costBefore} backward reference weight remain; recompile for the next suggestion"
    sb.append(s"    Effect: $verdict.\n")
    sb.toString
