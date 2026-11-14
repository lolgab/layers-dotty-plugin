package layers.plugin

import munit.FunSuite

class PackageMetricsTests extends FunSuite:

  private def edge(from: String, to: String, line: Int = 1) =
    Edge("F.scala", line, from, s"$from.X", to, s"$to.Y")

  private def facts(pkgs: Set[String], edges: List[Edge], types: Map[String, (Int, Int)] = Map.empty) =
    FileFacts("F.scala", pkgs, edges, types)

  private def metricsOf(f: FileFacts) =
    val graph = PackageGraph.build(List(f))
    (graph, PackageMetrics.compute(graph, List(f)))

  private val chain = facts(
    Set("app", "domain", "util", "lib1", "lib2"),
    List(edge("app", "domain"), edge("domain", "util"), edge("util", "lib1"), edge("util", "lib2")),
    Map("domain" -> (4, 1), "util" -> (2, 0))
  )

  test("afferent, efferent, instability, layers") {
    val (_, ms) = metricsOf(chain)
    val m = ms.map(x => x.pkg -> x).toMap
    assertEquals((m("domain").ca, m("domain").ce), (1, 1))
    assertEquals(m("domain").instability, 0.5)
    assertEquals(m("app").instability, 1.0)
    assertEquals(m("lib1").instability, 0.0)
    assertEquals(m("app").layer, 4)
    assertEquals(m("util").layer, 2)
    assertEquals(m("lib1").layer, 1)
  }

  test("abstractness and distance from the main sequence") {
    val (_, ms) = metricsOf(chain)
    val m = ms.map(x => x.pkg -> x).toMap
    assertEquals(m("domain").abstractness, Some(0.25))
    assertEquals(m("domain").distance, Some(math.abs(0.25 + 0.5 - 1)))
    assertEquals(m("app").abstractness, None)
    assertEquals(m("app").distance, None)
  }

  test("depending on a less stable package is a violation") {
    val (graph, ms) = metricsOf(chain)
    val vs = PackageMetrics.sdpViolations(graph, ms)
    assertEquals(vs.map(_.key), List("sdp domain -> util"))
    val text = PackageMetrics.render(vs.head, graph)
    assert(text.contains("Stable Dependencies violation: domain -> util"), text)
    assert(text.contains("lib1, lib2"), text)
    assert(text.contains("Dependency inversion"), text)
  }

  test("depending on a more stable package is fine") {
    val f = facts(Set("a", "b", "c"), List(edge("a", "b"), edge("a", "c"), edge("b", "c")))
    val (graph, ms) = metricsOf(f)
    assertEquals(PackageMetrics.sdpViolations(graph, ms), Nil)
  }

  test("layers are computed through cycles without looping") {
    val f = facts(Set("a", "b", "c"), List(edge("a", "b"), edge("b", "a"), edge("b", "c")))
    val (_, ms) = metricsOf(f)
    assertEquals(ms.map(m => m.pkg -> m.layer).toMap, Map("a" -> 2, "b" -> 2, "c" -> 1))
  }

  test("types survive the codec") {
    val f = facts(Set("a"), Nil, Map("a" -> (3, 1)))
    assertEquals(FileFacts.decode(FileFacts.encode(f)), Some(f))
  }

  test("facts of another version are rejected but still name their source") {
    val old = "layers-facts\t1\nsource\tx\npkg\ta\n"
    assertEquals(FileFacts.decode(old), None)
    assertEquals(FileFacts.peekSource(old), Some("x"))
    assertEquals(
      FileFacts.decode(s"layers-facts\t${FileFacts.version}\nsource\tx\npkg\ta\n"),
      Some(FileFacts("x", Set("a"), Nil))
    )
  }

  private val violation =
    val (graph, ms) = metricsOf(chain)
    PackageMetrics.sdpViolations(graph, ms).map(PackageMetrics.finding(_, graph))

  private def finding(key: String, value: Int) = Finding(key, value, "F.scala", 1, key)

  test("no baseline: every finding fails") {
    assertEquals(Baseline.evaluate(violation, None, update = false).failing, violation)
  }

  test("baselined findings pass") {
    val out = Baseline.evaluate(violation, Some(Map("sdp domain -> util" -> 0)), update = false)
    assertEquals(out.failing, Nil)
    assertEquals(out.stale, Nil)
  }

  test("resolved baseline entries are stale until locked in") {
    val out = Baseline.evaluate(Nil, Some(Map("sdp domain -> util" -> 0)), update = false)
    assertEquals(out.stale.size, 1)
    assert(out.stale.head.contains("fixed"), out.stale.head)
  }

  test("a baselined value may not get worse") {
    val base = Some(Map("cyclomatic a.B.m" -> 12))
    assertEquals(Baseline.evaluate(List(finding("cyclomatic a.B.m", 12)), base, update = false).failing, Nil)
    assertEquals(Baseline.evaluate(List(finding("cyclomatic a.B.m", 13)), base, update = false).failing.size, 1)
  }

  test("an improved value is stale until the baseline is lowered") {
    val base = Some(Map("cyclomatic a.B.m" -> 12))
    val out = Baseline.evaluate(List(finding("cyclomatic a.B.m", 11)), base, update = false)
    assertEquals(out.failing, Nil)
    assert(out.stale.head.contains("improved from 12 to 11"), out.stale.head)
    val updated = Baseline.evaluate(List(finding("cyclomatic a.B.m", 11)), base, update = true)
    assertEquals(updated.write, Some(Map("cyclomatic a.B.m" -> 11)))
  }

  test("update creates a missing baseline from current findings") {
    val out = Baseline.evaluate(violation, None, update = true)
    assertEquals(out.write, Some(Map("sdp domain -> util" -> 0)))
    assertEquals(out.failing, Nil)
  }

  test("update only tightens an existing baseline, never adds or raises") {
    val out = Baseline.evaluate(
      List(finding("cyclomatic a.B.m", 20), finding("lcom a.C", 3)),
      Some(Map("cyclomatic a.B.m" -> 12, "old" -> 1)),
      update = true
    )
    assertEquals(out.write, Some(Map("cyclomatic a.B.m" -> 12)))
    assertEquals(out.failing.map(_.key).toSet, Set("cyclomatic a.B.m", "lcom a.C"))
  }

  test("unchecked baseline keys are neither stale nor dropped") {
    val base = Some(Map("sdp a -> b" -> 0))
    val out = Baseline.evaluate(Nil, base, update = false, unchecked = _.startsWith("sdp "))
    assertEquals(out.stale, Nil)
    assertEquals(Baseline.evaluate(Nil, base, update = true, unchecked = _.startsWith("sdp ")).write, Some(Map("sdp a -> b" -> 0)))
  }

  test("baseline file round trip ignores comments") {
    val entries = Map("sdp b -> c" -> 0, "cyclomatic a.B.m" -> 12)
    assertEquals(Baseline.parse(Baseline.render(entries)), entries)
  }
