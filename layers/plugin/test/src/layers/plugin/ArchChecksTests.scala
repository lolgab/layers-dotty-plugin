package layers.plugin

import munit.FunSuite

class ArchChecksTests extends FunSuite:

  private def pkgFacts(pkg: String, deps: List[String] = Nil) =
    FileFacts(
      s"$pkg.scala", Set(pkg),
      deps.map(d => Edge(s"$pkg.scala", 1, pkg, s"$pkg.A", d, s"$d.B")),
      Map(pkg -> (1, 0))
    )

  private def domain(options: List[String], facts: List[FileFacts]): Set[String] =
    Domains.infer(ArchConfig.fromOptions(options), facts, PackageGraph.build(facts))

  test("options are read, malformed ones ignored") {
    val c = ArchConfig.fromOptions(List(
      "domain=a.model, a.core", "identityTypes=Tenant", "pureExempt=a.util.Today", "layerKinds=a.x:web,a.y:none,broken",
      "godTypeMinFields=15", "minTransitionSites=zero", "ruleByRelationship", "maxLayersPerMethod=4"
    ))
    assertEquals(c.domain, List("a.model", "a.core"))
    assertEquals(c.identityTypes, List("Tenant"))
    assertEquals(c.layerKinds, Map("a.x" -> "web", "a.y" -> "none"))
    assertEquals(c.godTypeMinFields, 15)
    assertEquals(c.minTransitionSites, 3)
    assertEquals(c.maxLayersPerMethod, 4)
    assert(c.ruleByRelationship && ArchConfig.fromOptions(Nil).ruleByRelationship == false)
  }

  test("a package called model or domain that depends on no infrastructure is the domain") {
    val fs = List(pkgFacts("app.model"), pkgFacts("app.database", List("app.model")), pkgFacts("app.feature", List("app.model")))
    assertEquals(domain(Nil, fs), Set("app.model"))
  }

  test("a package that depends on the database, or is infrastructure by name, is not domain") {
    val fs = List(pkgFacts("app.database"), pkgFacts("app.model", List("app.database")), pkgFacts("app.rendering"))
    assertEquals(domain(Nil, fs), Set.empty[String])
  }

  test("a core used by at least three packages is domain even under another name; support packages and auth are not") {
    val users = List("app.a", "app.b", "app.c").map(p => pkgFacts(p, List("app.core2", "app.util", "app.json", "app.auth")))
    val fs = users ++ List(pkgFacts("app.core2"), pkgFacts("app.util"), pkgFacts("app.json"), pkgFacts("app.auth"))
    assertEquals(domain(Nil, fs), Set("app.core2"))
    assertEquals(domain(List("domainMinDependents=5"), fs), Set.empty[String])
  }

  test("domain= names packages and their subpackages explicitly") {
    val fs = List(pkgFacts("app.model"), pkgFacts("app.model.sub"), pkgFacts("app.other"))
    assertEquals(domain(List("domain=app.model"), fs), Set("app.model", "app.model.sub"))
  }

  test("layerKinds can make a package infrastructure, so what depends on it is not domain") {
    val fs = List(pkgFacts("app.things"), pkgFacts("app.model", List("app.things")))
    assertEquals(domain(Nil, fs), Set("app.model"))
    assertEquals(domain(List("layerKinds=app.things:persistence"), fs), Set.empty[String])
  }

  test("package kinds come from names and libraries") {
    val cfg = ArchConfig()
    assertEquals(PackageKinds.kind("app.database", cfg, internal = true), Some("persistence"))
    assertEquals(PackageKinds.kind("app.rendering", cfg, internal = true), Some("rendering"))
    assertEquals(PackageKinds.kind("app.routes", cfg, internal = true), Some("web"))
    assertEquals(PackageKinds.kind("app.stripe", cfg, internal = true), Some("integration"))
    assertEquals(PackageKinds.kind("app.model", cfg, internal = true), None)
    assertEquals(PackageKinds.kind("scalatags.generic", cfg, internal = false), Some("rendering"))
    assertEquals(PackageKinds.kind("com.augustnagro.magnum", cfg, internal = false), Some("persistence"))
    assertEquals(PackageKinds.kind("ujson", cfg, internal = false), None)
  }

  test("facts extracted under other identityTypes or layerKinds options are stale") {
    val cur = ArchConfig.fromOptions(List("identityTypes=Tenant"))
    val ok = FileFacts("A.scala", Set("p"), Nil, arch = List(ArchFact("cfg", "A.scala", 0, List(cur.extractionKey))))
    val old = FileFacts("B.scala", Set("p"), Nil, arch = List(ArchFact("cfg", "B.scala", 0, List(ArchConfig().extractionKey))))
    assertEquals(ArchChecks.stale(List(ok, old), cur), List("B.scala"))
  }

  test("architecture findings are ratcheted by the lines of their sites, and class ones by their declaration") {
    val changes = GitSignals.Changes(Map("/r/A.scala" -> List((10, 12))), Set("/r/New.scala"))
    val purity = Finding("domain-purity a.A", 2, "/r/A.scala", 5, "m", List((5, 5), (11, 11)))
    val purityOld = Finding("domain-purity a.B", 1, "/r/A.scala", 40, "m", List((40, 40)))
    val god = Finding("god-type a.G", 12, "/r/A.scala", 30, "m")
    val mix = Finding("layer-mix a.A.run", 4, "/r/A.scala", 1, "m", List((1, 8), (10, 14)))
    val (fail, existing) = Ratchet.split(List(purity, purityOld, god, mix), changes)
    assertEquals(fail.map(_.key), List("domain-purity a.A", "layer-mix a.A.run"))
    assertEquals(existing.map(_.key), List("domain-purity a.B", "god-type a.G"))
  }

  test("all architecture facts survive the codec") {
    val rows = List(
      ArchFact("impure", "F.scala", 3, List("p.A.m", "clock", "LocalDate.now", "p", "p.A")),
      ArchFact("ref", "F.scala", 4, List("p.A.m", "p", "q", "persistence", ArchFact.join(List("Db.load", "Db.save")), "2")),
      ArchFact("identparam", "F.scala", 5, List("p.A.m", "user", "p.User", "0", "1")),
      ArchFact("classinfo", "F.scala", 6, List("p.C", "p", "case", "12", ArchFact.join(List("a", "b")))),
      ArchFact("copy", "F.scala", 7, List("p.A.m", "p.A", "p.C", "status", "\"a b\"")),
      ArchFact("cfg", "F.scala", 0, List("|"))
    )
    val f = FileFacts("F.scala", Set("p"), Nil, arch = rows)
    assertEquals(FileFacts.decode(FileFacts.encode(f)).map(_.copy(source = "F.scala")), Some(f))
  }

  test("a malformed architecture line makes the file's facts unusable instead of silently dropping them") {
    val good = FileFacts.encode(FileFacts("F.scala", Set("p"), Nil, arch = List(ArchFact("cfg", "F.scala", 0, List("x")))))
    assert(FileFacts.decode(good).isDefined)
    assertEquals(FileFacts.decode(good.replace("arch\tcfg\t0", "arch\tcfg\tNaN")), None)
  }
