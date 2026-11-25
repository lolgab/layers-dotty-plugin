package layers.plugin

import munit.FunSuite

class SliceCouplingTests extends FunSuite:

  private def decl(pkg: String, name: String) =
    TypeDecl(s"/src/$pkg/$name.scala", 1, s"$pkg.$name", pkg, "class", false, false, false, Nil)

  private def dep(from: String, to: String, line: Int = 1) =
    val (fp, ft) = from.splitAt(from.lastIndexOf('.'))
    val (tp, tt) = to.splitAt(to.lastIndexOf('.'))
    ClassDep(s"/src/$fp/${ft.drop(1)}.scala", line, from, to, fp, tp, 1)

  private def facts(types: List[TypeDecl], deps: List[ClassDep]) =
    val edges = deps.filter(d => d.fromPkg != d.toPkg).map(d => Edge(d.file, d.line, d.fromPkg, d.fromTop, d.toPkg, d.toTop))
    val pkgs = (types.map(_.pkg) ++ deps.flatMap(d => List(d.fromPkg, d.toPkg))).toSet
    FileFacts("/src/All.scala", pkgs, edges, typeDecls = types, classDeps = deps)

  private def check(f: FileFacts, opts: String*) =
    SliceChecks.findings(List(f), PackageGraph.build(List(f)), opts.toList)

  /** hub `domain` used by u1..u12; each user is a slice that also uses its own helper. */
  private def hubFacts(types: Int, users: Int = 12, unstable: Boolean = false) =
    val domainTypes = (1 to types).map(i => decl("domain", s"K$i")).toList
    val userTypes = (1 to users).map(i => decl(s"u$i", s"U$i")).toList
    val uses = (1 to users).flatMap(i => List(dep(s"u$i.U$i", s"domain.K${(i % types) + 1}"), dep(s"u$i.U$i", s"domain.K${((i + 1) % types) + 1}"))).toList
    val outgoing = if unstable then (1 to 6).map(i => dep("domain.K1", s"x$i.X$i")).toList else Nil
    val xs = if unstable then (1 to 6).map(i => decl(s"x$i", s"X$i")).toList else Nil
    facts(domainTypes ++ userTypes ++ xs, uses ++ outgoing)

  test("a slice depending on more than the limit of other slices is a finding that lists the cheapest cuts") {
    val targets = (1 to 8).map(i => decl(s"t$i", s"T$i")).toList
    // t1 is used from one place, t8 from five: t1 is the cheapest to cut
    val deps = (1 to 8).flatMap(i => (1 to (if i == 8 then 5 else 1 + i % 3)).map(l => dep("hub.H", s"t$i.T$i", l * 10 + i))).toList
    val fs = check(facts(decl("hub", "H") :: targets, deps)).filter(_.kind == "slice-fanout")
    assertEquals(fs.map(_.key), List("slice-fanout hub"))
    assertEquals(fs.head.value, 8)
    val m = fs.head.message
    assert(m.contains("depends on 8 other slices (limit 6)"), m)
    assert(m.contains("Cut at least 2"), m)
    assert(m.indexOf("hub -> t1") < m.indexOf("hub -> t8") || !m.contains("hub -> t8"), m)
    assert(m.contains("Fix each dependency") && m.contains("Done when"), m)
  }

  test("six other slices are fine, and the limit is an option") {
    val targets = (1 to 6).map(i => decl(s"t$i", s"T$i")).toList
    val deps = (1 to 6).map(i => dep("hub.H", s"t$i.T$i")).toList
    val f = facts(decl("hub", "H") :: targets, deps)
    assertEquals(check(f).filter(_.kind == "slice-fanout"), Nil)
    assertEquals(check(f, "maxSliceFanOut=5").map(_.key), List("slice-fanout hub"))
  }

  test("shared packages and its own subpackages are not other slices") {
    val targets = (1 to 3).map(i => decl(s"t$i", s"T$i")).toList ++ (1 to 5).map(i => decl(s"util.u$i", s"U$i"))
    val deps = (1 to 3).map(i => dep("hub.H", s"t$i.T$i")).toList ++ (1 to 5).map(i => dep("hub.H", s"util.u$i.U$i")) :+
      dep("hub.H", "hub.sub.S")
    val f = facts(decl("hub", "H") :: decl("hub.sub", "S") :: targets, deps)
    assertEquals(check(f).filter(_.kind == "slice-fanout"), Nil)
  }

  test("a package that holds several slices is a namespace, not their family") {
    val ts = (1 to 8).map(i => decl(s"app.t$i", s"T$i")).toList
    val deps = (1 to 8).map(i => dep("app.Main", s"app.t$i.T$i")).toList
    val f = facts(decl("app", "Main") :: ts, deps)
    assertEquals(check(f).filter(_.kind == "slice-fanout").map(_.key), List("slice-fanout app"))
  }

  test("moving the references behind a wrapper slice does not hide the dependency count of the wrapper") {
    // hub -> w -> t1..t8: w now has the fan-out
    val ts = (1 to 8).map(i => decl(s"t$i", s"T$i")).toList
    val deps = dep("hub.H", "w.W") :: (1 to 8).map(i => dep("w.W", s"t$i.T$i")).toList
    val fs = check(facts(decl("hub", "H") :: decl("w", "W") :: ts, deps)).filter(_.kind == "slice-fanout")
    assertEquals(fs.map(_.key), List("slice-fanout w"))
  }

  test("a hub that is a big grab-bag is a finding that says which slice owns which type") {
    val fs = check(hubFacts(types = 45), "maxHubTypes=40").filter(_.kind == "slice-hub-grabbag")
    assertEquals(fs.map(_.key), List("slice-hub-grabbag domain"))
    assertEquals(fs.head.value, 45)
    assert(fs.head.message.contains("12 slices depend on it (limit 10)") && fs.head.message.contains("45 top-level types"), fs.head.message)
    assert(fs.head.message.contains("used by one slice only"), fs.head.message)
  }

  test("a small stable hub is a legitimate domain; so is a big package that few slices use") {
    assertEquals(check(hubFacts(types = 8)).filter(_.kind.startsWith("slice-hub")), Nil)
    assertEquals(check(hubFacts(types = 45, users = 9), "maxHubTypes=40").filter(_.kind.startsWith("slice-hub")), Nil)
  }

  test("a hub that depends on a lot is unstable") {
    val fs = check(hubFacts(types = 8, unstable = true), "maxHubInstability=0.2").filter(_.kind == "slice-hub-unstable")
    assertEquals(fs.map(_.key), List("slice-hub-unstable domain"))
    assert(fs.head.message.contains("it depends on x1"), fs.head.message)
    assertEquals(check(hubFacts(types = 8, unstable = true)).filter(_.kind == "slice-hub-unstable").size, 1) // 6/(12+6)=0.33 > 0.3
  }

  test("fan-in and fan-out findings are not at the same position and carry a whole-file range for the since ratchet") {
    val f = check(hubFacts(types = 45, unstable = true), "maxHubTypes=40").filter(_.kind.startsWith("slice-hub"))
    assertEquals(f.map(x => (x.file, x.line)).distinct.size, f.size)
    assert(f.forall(_.lines == List((1, Int.MaxValue))))
  }
