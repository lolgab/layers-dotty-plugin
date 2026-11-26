package layers.plugin

import munit.FunSuite

class PackageCyclesTests extends FunSuite:

  private def edge(from: String, to: String, line: Int = 1, file: String = "F.scala") =
    Edge(file, line, from, s"$from.X", to, s"$to.Y")

  private def facts(pkgs: Set[String], edges: Edge*) = FileFacts("F.scala", pkgs, edges.toList)

  test("acyclic graph has no cycles") {
    val f = facts(Set("a", "b", "c"), edge("a", "b"), edge("b", "c"), edge("a", "c"))
    assertEquals(PackageCycles.analyze(List(f)), Nil)
  }

  test("external packages are ignored") {
    val f = facts(Set("a"), edge("a", "lib"), edge("lib", "a"))
    assertEquals(PackageCycles.analyze(List(f)), Nil)
  }

  test("two package cycle cuts the edge with fewer references") {
    val f = facts(
      Set("a", "b"),
      edge("a", "b", 1),
      edge("a", "b", 2).copy(fromSym = "a.Other"),
      edge("b", "a", 3)
    )
    val cycles = PackageCycles.analyze(List(f))
    assertEquals(cycles.size, 1)
    val c = cycles.head
    assertEquals(c.packages, List("a", "b"))
    assertEquals(c.cut.map(x => (x.from, x.to)), List(("b", "a")))
    assertEquals(c.order, List("a", "b"))
  }

  test("facts from several files are merged") {
    val f1 = FileFacts("A.scala", Set("a"), List(edge("a", "b", file = "A.scala")))
    val f2 = FileFacts("B.scala", Set("b"), List(edge("b", "a", file = "B.scala")))
    assertEquals(PackageCycles.analyze(List(f1, f2)).map(_.packages), List(List("a", "b")))
  }

  test("three package cycle cuts a single cheapest edge") {
    val f = facts(
      Set("a", "b", "c"),
      edge("a", "b"),
      edge("b", "c"),
      edge("c", "a")
    )
    val c = PackageCycles.analyze(List(f)).head
    assertEquals(c.cut.size, 1)
    assertEquals(c.edgeCount, 3)
  }

  test("large cycle falls back to greedy ordering and still breaks every cycle") {
    val names = (0 until 24).map(i => f"p$i%02d").toList
    val ring = names.zip(names.tail :+ names.head).map { case (a, b) => edge(a, b) }
    val c = PackageCycles.analyze(List(facts(names.toSet, ring*))).head
    assertEquals(c.cut.size, 1)
    assertEquals(c.order.toSet, names.toSet)
  }

  test("two separate cycles are reported separately") {
    val f = facts(
      Set("a", "b", "c", "d"),
      edge("a", "b"), edge("b", "a"),
      edge("c", "d"), edge("d", "c")
    )
    assertEquals(PackageCycles.analyze(List(f)).map(_.packages), List(List("a", "b"), List("c", "d")))
  }

  test("render names edges, sites, and a fix") {
    val f = facts(Set("a", "b"), edge("a", "b", 10, "src/A.scala"), edge("b", "a", 20, "src/B.scala"))
    val text = PackageCycles.render(PackageCycles.analyze(List(f)).head)
    assert(text.contains("Cyclic dependency between packages: a, b"), text)
    assert(text.contains("src/"), text)
    assert(text.contains("Dependency inversion"), text)
    assert(text.contains("Done when"), text)
  }

  test("facts round trip through the codec") {
    val f = FileFacts("/x/A.scala", Set("a", "a.b"), List(edge("a", "b", 7, "/x/A.scala")))
    assertEquals(FileFacts.decode(FileFacts.encode(f)), Some(f))
  }

  test("corrupt facts are rejected") {
    assertEquals(FileFacts.decode("garbage"), None)
    assertEquals(FileFacts.decode("layers-facts\t1\nsource\tx\nedge\tnotanumber\ta\tb\tc\td\n"), None)
  }

  test("facts store: reuses current facts, reports outdated ones, drops deleted sources") {
    val dir = java.nio.file.Files.createTempDirectory("facts-")
    val src = java.nio.file.Files.createTempFile("Src", ".scala")
    val store = new FactsStore(dir)
    store.write(FileFacts(src.toString, Set("a"), Nil))
    assertEquals(store.loadOthers(Set.empty).facts.map(_.source), List(src.toString))
    assertEquals(store.loadOthers(Set(src.toString)), FactsStore.Loaded(Nil, Nil))

    java.nio.file.Files.list(dir).forEach { f =>
      java.nio.file.Files.writeString(f, s"layers-facts\t1\nsource\t$src\npkg\ta\n")
    }
    assertEquals(store.loadOthers(Set.empty), FactsStore.Loaded(Nil, List(src.toString)))

    java.nio.file.Files.delete(src)
    assertEquals(store.loadOthers(Set.empty), FactsStore.Loaded(Nil, Nil))
    assertEquals(java.nio.file.Files.list(dir).count(), 0L)
  }
