package layers.plugin

import munit.FunSuite

class ClassMetricsTests extends FunSuite:

  private val ok = MethodFact("F.scala", 1, "a.B.m", 1, 0, 3, 1, 0)
  private val t = Thresholds()

  test("measurements within limits produce no findings") {
    assertEquals(ClassMetrics.findings(List(ok), Nil, t), Nil)
  }

  test("every exceeded limit produces its own finding with value and fix") {
    val bad = ok.copy(cyclomatic = 11, cognitive = 16, lines = 61, params = 6, nesting = 5)
    val fs = ClassMetrics.findings(List(bad), Nil, t)
    assertEquals(fs.map(_.key).toSet, Set("cyclomatic a.B.m", "cognitive a.B.m", "lines a.B.m", "params a.B.m", "nesting a.B.m"))
    val cyc = fs.find(_.key.startsWith("cyclomatic")).get
    assertEquals(cyc.value, 11)
    assert(cyc.message.contains("limit 10"), cyc.message)
    assert(cyc.message.contains("Fix with one of"), cyc.message)
  }

  test("limit itself is allowed") {
    val edge = ok.copy(cyclomatic = 10, cognitive = 15, lines = 60, params = 5, nesting = 4)
    assertEquals(ClassMetrics.findings(List(edge), Nil, t), Nil)
  }

  test("overloads are merged keeping the worst") {
    val fs = ClassMetrics.findings(List(ok.copy(cyclomatic = 11), ok.copy(cyclomatic = 15, line = 9)), Nil, t)
    assertEquals(fs.map(_.value), List(15))
  }

  test("class with more groups than the limit is reported with the groups") {
    val c = ClassFact("F.scala", 3, "a.C", 5, List(List("x", "y"), List("z"), List("w")))
    val fs = ClassMetrics.findings(Nil, List(c), t)
    assertEquals(fs.map(_.key), List("lcom a.C"))
    assert(fs.head.message.contains("1. x, y"), fs.head.message)
    assert(ClassMetrics.findings(Nil, List(c.copy(clusters = List(List("x"), List("z")))), t).isEmpty)
  }

  test("thresholds come from options") {
    val th = Thresholds.fromOptions(List("design", "maxCyclomatic=3", "maxLcom=x", "maxParams=0"))
    assertEquals(th, Thresholds(maxCyclomatic = 3))
  }

  test("method and class facts survive the codec") {
    val f = FileFacts(
      "/x/A.scala", Set("a"), Nil, Map.empty,
      List(MethodFact("/x/A.scala", 4, "a.B.m", 3, 5, 12, 2, 1)),
      List(ClassFact("/x/A.scala", 2, "a.B", 4, List(List("x", "y"), List("z"))))
    )
    assertEquals(FileFacts.decode(FileFacts.encode(f)), Some(f))
  }
