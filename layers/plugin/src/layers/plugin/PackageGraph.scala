package layers.plugin

import scala.collection.mutable

/** The dependency graph between the project's own packages, merged from the facts of all source files.
  *
  * @param sites
  *   every reference behind each package edge `from -> to`, sorted by file and line
  */
final class PackageGraph(val nodes: List[String], val sites: Map[(String, String), List[Edge]]):

  val succ: Map[String, List[String]] = sites.keys.toList.groupMap(_._1)(_._2).view.mapValues(_.sorted).toMap
  val pred: Map[String, List[String]] = sites.keys.toList.groupMap(_._2)(_._1).view.mapValues(_.sorted).toMap

  /** Packages this one depends on. */
  def efferent(pkg: String): Int = succ.getOrElse(pkg, Nil).size

  /** Packages depending on this one. */
  def afferent(pkg: String): Int = pred.getOrElse(pkg, Nil).size

  /** Strongly connected components (Tarjan), deterministic. Components of size > 1 are cycles. */
  lazy val components: List[List[String]] =
    var counter = 0
    val index = mutable.Map[String, Int]()
    val low = mutable.Map[String, Int]()
    val stack = mutable.Stack[String]()
    val onStack = mutable.Set[String]()
    val result = mutable.ListBuffer[List[String]]()

    def visit(v: String): Unit =
      index(v) = counter
      low(v) = counter
      counter += 1
      stack.push(v)
      onStack += v
      for w <- succ.getOrElse(v, Nil) do
        if !index.contains(w) then
          visit(w)
          low(v) = math.min(low(v), low(w))
        else if onStack(w) then low(v) = math.min(low(v), index(w))
      if low(v) == index(v) then
        val comp = mutable.ListBuffer[String]()
        var w = ""
        while
          w = stack.pop()
          onStack -= w
          comp += w
          w != v
        do ()
        result += comp.toList

    for n <- nodes if !index.contains(n) do visit(n)
    result.toList

object PackageGraph:

  def build(facts: Iterable[FileFacts]): PackageGraph =
    val internal = facts.flatMap(_.packages).toSet
    val sites = mutable.Map[(String, String), mutable.ListBuffer[Edge]]()
    for f <- facts; e <- f.edges do
      if e.fromPkg != e.toPkg && internal(e.fromPkg) && internal(e.toPkg) then
        sites.getOrElseUpdate((e.fromPkg, e.toPkg), mutable.ListBuffer()) += e
    val sorted = sites.view.mapValues(_.toList.sortBy(e => (e.file, e.line, e.fromSym, e.toSym))).toMap
    new PackageGraph(internal.toList.sorted, sorted)
