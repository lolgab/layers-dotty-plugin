package layers.plugin

import munit.FunSuite

class MovePlanTests extends FunSuite:

  private def dep(from: String, to: String, fromPkg: String, toPkg: String, refs: Int = 1) =
    ClassDep("F.scala", 1, from, to, fromPkg, toPkg, refs)

  private def edgeOf(d: ClassDep) =
    Edge(d.file, d.line, d.fromPkg, d.fromTop, d.toPkg, d.toTop)

  private def facts(deps: ClassDep*) =
    FileFacts(
      "F.scala",
      deps.flatMap(d => List(d.fromPkg, d.toPkg)).toSet,
      deps.filter(d => d.fromPkg != d.toPkg).map(edgeOf).toList,
      classDeps = deps.toList
    )

  private def planFor(f: FileFacts) =
    val graph = PackageGraph.build(List(f))
    val cycle = PackageCycles.analyze(graph).head
    (cycle, MovePlan.suggest(graph, cycle, List(f)))

  test("a shared class used by the other package is extracted") {
    // app.Api uses auth.Auth, auth.Auth uses app.User (the cycle). User uses nothing.
    val f = facts(
      dep("app.Api", "auth.Auth", "app", "auth", 3),
      dep("auth.Auth", "app.User", "auth", "app", 4),
      dep("app.Api", "app.User", "app", "app", 2)
    )
    val (_, plan) = planFor(f)
    val p = plan.get
    assertEquals(p.moves.map(_.classes), List(List("app.User")))
    assertEquals(p.moves.head.from, List(("app", 1)))
    assertEquals(p.moves.head.usedBy, List("app", "auth"))
    assertEquals(p.costAfter, 0)
  }

  test("what the moved class uses inside the cycle moves with it") {
    val f = facts(
      dep("app.Api", "auth.Auth", "app", "auth"),
      dep("auth.Auth", "app.User", "auth", "app", 5),
      dep("app.User", "app.Money", "app", "app"),
      dep("app.Api", "app.Other", "app", "app")
    )
    val (_, plan) = planFor(f)
    assertEquals(plan.get.moves.head.classes, List("app.Money", "app.User"))
  }

  test("no plan when no move helps") {
    // two classes that really depend on each other across packages
    val f = facts(dep("a.A", "b.B", "a", "b"), dep("b.B", "a.A", "b", "a"))
    val (_, plan) = planFor(f)
    // moving either class alone drags the other with it, which is more than the share limit allows
    assertEquals(plan, None)
  }

  test("render lists classes, files, usage and effect") {
    val f = facts(
      dep("app.Api", "auth.Auth", "app", "auth"),
      dep("auth.Auth", "app.User", "auth", "app", 4)
    )
    val (cycle, plan) = planFor(f)
    val text = PackageCycles.render(cycle, plan)
    assert(text.contains("Suggested restructuring"), text)
    assert(text.contains("app.User"), text)
    assert(text.contains("Used by:"), text)
    assert(text.contains("the cycle is gone"), text)
  }

  test("render without a plan is unchanged") {
    val f = facts(dep("a.A", "b.B", "a", "b"), dep("b.B", "a.A", "b", "a"))
    val (cycle, _) = planFor(f)
    assert(!PackageCycles.render(cycle, None).contains("Suggested restructuring"))
  }

  test("class deps survive the codec") {
    val f = facts(dep("a.A", "b.B", "a", "b", 3))
    assertEquals(FileFacts.decode(FileFacts.encode(f)).map(_.copy(source = "F.scala")), Some(f))
  }
