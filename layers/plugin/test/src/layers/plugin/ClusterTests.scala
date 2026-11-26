package layers.plugin

import munit.FunSuite

/** Pieces that are each fine are fine, up to a cap; cutting a long method into pieces that are not, is still caught. */
class ClusterTests extends FunSuite:

  private val t = Thresholds()

  private def fact(sym: String, size: Int = 3, cyc: Int = 1, cog: Int = 0, calls: List[String] = Nil, file: String = "F.scala", pkg: String = "a") =
    MethodFact(file, 1, sym, cyc, cog, size, size, 1, 0, false, calls, false, pkg)

  private def cluster(root: MethodFact, helpers: Int, size: Int, cyc: Int, cog: Int) =
    val hs = (1 to helpers).map(i => fact(s"a.H.h$i", size, cyc, cog, file = "H.scala")).toList
    root.copy(calls = hs.map(_.sym)) :: hs

  test("a page of cohesive sections, each within every limit, is not judged by its sum") {
    // 8 sections of 30 lines: 245 lines, 17 paths, cognitive 32 in all, each section fine on its own
    val ms = cluster(fact("a.R.page", 5), 8, 30, 3, 4)
    assertEquals(ClassMetrics.findings(ms, Nil, t), Nil)
  }

  test("the sum is judged again above the caps") {
    val ms = cluster(fact("a.R.page", 5), 8, 40, 3, 4) // 325 lines
    assertEquals(ClassMetrics.findings(ms, Nil, t).map(_.key).toSet, Set("lines a.R.page", "cognitive a.R.page", "cyclomatic a.R.page"))
    assertEquals(ClassMetrics.findings(ms, Nil, t.copy(maxClusterLines = 400)), Nil)
  }

  test("five helpers of dense logic add up their paths even if each is fine") {
    // 5 x (60 lines, 7 paths, flat enough for the long limit): 31 paths
    val ms = cluster(fact("a.R.run", 3), 5, 60, 7, 4)
    val keys = ClassMetrics.findings(ms, Nil, t).map(_.key).toSet
    assert(keys.contains("cyclomatic a.R.run"), keys.toString)
    assertEquals(ClassMetrics.findings(ms, Nil, t.copy(maxClusterCyclomatic = 31, maxClusterLines = 400)), Nil)
  }

  test("a long method cut into pieces that are each too long is not saved by the cap") {
    val ms = cluster(fact("a.R.run", 3), 3, 70, 2, 8)
    val keys = ClassMetrics.findings(ms, Nil, t).map(_.key).toSet
    // each piece is reported on its own, none disappears into a cluster
    assertEquals(keys, Set("lines a.H.h1", "lines a.H.h2", "lines a.H.h3"))
  }

  test("one piece over a limit keeps the rest of the cluster summed") {
    val hs = List(fact("a.H.big", 70, 2, 8, file = "H.scala"), fact("a.H.x", 40, 3, 4, file = "H.scala"), fact("a.H.y", 40, 3, 4, file = "H.scala"))
    val ms = fact("a.R.run", 5, calls = hs.map(_.sym)) :: hs
    // x and y are fine, big is reported itself; run + x + y is 85 lines of fine pieces
    assertEquals(ClassMetrics.findings(ms, Nil, t).map(_.key), List("lines a.H.big"))
  }

  test("the caps are options") {
    val th = Thresholds.fromOptions(List("maxClusterLines=500", "maxClusterCyclomatic=x"))
    assertEquals(th.maxClusterLines, 500)
    assertEquals(th.maxClusterCyclomatic, 30)
  }

  private def design(source: String, opts: String*) =
    ArchTestSupport.design(List("Layer.scala" -> "package p\nobject layer\n", "Src.scala" -> source), opts*)

  test("compiled: three fine helpers, summed paths over the method limit but under the cluster cap") {
    val branch = "    if x == 1 then 1 else if x == 2 then 2 else if x == 3 then 3 else 0"
    val src = s"""package p
object C:
  def run(x: Int): Int = one(x) + two(x) + three(x)
  private def one(x: Int): Int =
$branch
  private def two(x: Int): Int =
$branch
  private def three(x: Int): Int =
$branch
"""
    assert(!design(src, "maxCyclomatic=6").hasErrors, design(src, "maxCyclomatic=6").errorMessages)
    assert(design(src, "maxCyclomatic=6", "maxClusterCyclomatic=8").errorMessages.contains("Method `p.C$.run`"))
  }

  test("a by-name fallback that is a plain call or reference does not add a nesting level") {
    def nesting(fallback: String, limit: Int) =
      design(
        s"""package p
object C:
  def notFound(s: String): Int = s.length
  def run(o: Option[Int], x: Int): Int =
    if x > 0 then
      if x > 1 then o.getOrElse($fallback) else 0
    else 0
""",
        s"maxNesting=$limit"
      ).errorMessages
    assert(!nesting("notFound(\"x\")", 2).contains("nested too deeply"))
    assert(!nesting("x", 2).contains("nested too deeply"))
    // a branching fallback is nested
    assert(nesting("if x > 5 then 1 else 2", 2).contains("nested too deeply"))
  }
