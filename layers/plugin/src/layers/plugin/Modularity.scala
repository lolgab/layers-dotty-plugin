package layers.plugin

/** Community detection on a weighted undirected graph with the Louvain method: nodes start alone, move to the
  * neighbouring group that most increases modularity until none moves, then groups are contracted into nodes and the
  * process repeats. Deterministic: nodes are visited in index order and ties keep the current group.
  */
object Modularity:

  private type Graph = Map[(Int, Int), Double]

  /** One pass of local moving; returns the community of each node and whether anything moved. */
  private def localMoving(n: Int, weights: Graph, maxSweeps: Int): (Array[Int], Boolean) =
    val community = Array.tabulate(n)(identity)
    val neighbours = Array.fill(n)(collection.mutable.Map[Int, Double]().withDefaultValue(0.0))
    for ((a, b), w) <- weights if w > 0 do
      if a == b then neighbours(a)(a) += 2 * w // a self loop counts twice in the degree
      else
        neighbours(a)(b) += w
        neighbours(b)(a) += w
    val degree = Array.tabulate(n)(i => neighbours(i).values.sum)
    val twoM = degree.sum
    var anyMoved = false
    if twoM > 0 then
      val total = degree.clone() // sum of degrees per community
      var moved = true
      var sweep = 0
      while moved && sweep < maxSweeps do
        moved = false
        sweep += 1
        for i <- 0 until n if degree(i) > 0 do
          val own = community(i)
          val toCommunity = collection.mutable.Map[Int, Double]().withDefaultValue(0.0)
          for (j, w) <- neighbours(i) if j != i do toCommunity(community(j)) += w
          total(own) -= degree(i)
          // gain of joining community c, up to a factor shared by all candidates
          def gain(c: Int) = toCommunity(c) - total(c) * degree(i) / twoM
          var best = own
          var bestGain = gain(own)
          for c <- toCommunity.keys.toList.sorted if gain(c) > bestGain + 1e-12 do
            best = c
            bestGain = gain(c)
          total(best) += degree(i)
          if best != own then
            community(i) = best
            moved = true
            anyMoved = true
    (community, anyMoved)

  /** @param weights
    *   edge weights keyed by (low index, high index)
    * @return
    *   the community number of each node
    */
  def communities(n: Int, weights: Map[(Int, Int), Double], maxSweeps: Int = 25): Array[Int] =
    var nodeOf = Array.tabulate(n)(identity) // original node -> node of the current (contracted) graph
    var size = n
    var graph: Graph = weights
    var continue = true
    while continue do
      val (community, moved) = localMoving(size, graph, maxSweeps)
      if !moved then continue = false
      else
        val ids = community.distinct.sorted.zipWithIndex.toMap
        nodeOf = nodeOf.map(x => ids(community(x)))
        val contracted = collection.mutable.Map[(Int, Int), Double]().withDefaultValue(0.0)
        for ((a, b), w) <- graph do
          val (ca, cb) = (ids(community(a)), ids(community(b)))
          contracted((math.min(ca, cb), math.max(ca, cb))) += w
        graph = contracted.toMap
        size = ids.size
    nodeOf
