package layers.plugin

import munit.FunSuite

class ChangeScatterTests extends FunSuite:

  private val root = "/repo/src/"

  private def file(path: String, pkg: String, tops: String*) =
    FileFacts(
      root + path, Set(pkg), Nil,
      typeDecls = tops.toList.map(t => TypeDecl(root + path, 1, s"$pkg.$t", pkg, "class", false, false, false, Nil))
    )

  private def commit(paths: String*) = ChangeScatter.Commit(paths.map(root + _).toSet, paths.size)

  private def graphOf(facts: List[FileFacts], edges: (String, String)*) =
    val sites = edges.map { case (f, t) => (f, t) -> List(Edge(root + f, 1, f, "x", t, "y")) }.toMap
    new PackageGraph(facts.flatMap(_.packages).distinct.sorted, sites)

  private val o = ChangeScatter.Options("500 commits", minUsable = 10)

  private def run(commits: List[ChangeScatter.Commit], facts: List[FileFacts], edges: (String, String)*) =
    ChangeScatter.analyze(commits, facts, graphOf(facts, edges*), o)

  private val three = List(file("a/A.scala", "a", "A"), file("b/B.scala", "b", "B"), file("c/C.scala", "c", "C"))

  /** 10 commits changing a and b together, 12 changing only c, 2 changing only a. */
  private val together = List.fill(10)(commit("a/A.scala", "b/B.scala")) ++ List.fill(12)(commit("c/C.scala")) ++ List.fill(2)(commit("a/A.scala"))

  test("packages that keep changing together are a finding with the files, the numbers and fixes") {
    val r = run(together, three)
    assertEquals(r.findings.map(_.key), List("co-change a <-> b"))
    val f = r.findings.head
    assertEquals(f.value, 100)
    assert(f.message.contains("10 of the 10 commits"), f.message)
    assert(f.message.contains("24") || f.message.contains("commits used"), f.message)
    assert(f.message.contains("a/A.scala") && f.message.contains("b/B.scala"), f.message)
    assert(f.message.contains("Fix with one of") && f.message.contains("vertical slice"), f.message)
    assert(r.checked && r.note.exists(_.contains("24 commits used")), r.note)
    assertEquals(f.file, root + "a/A.scala")
  }

  test("the result is deterministic") {
    assertEquals(run(together, three).findings.map(f => (f.key, f.value, f.message)), run(together, three).findings.map(f => (f.key, f.value, f.message)))
    assertEquals(run(together.reverse, three).findings.map(_.key), List("co-change a <-> b"))
  }

  test("too few joint commits is not a finding") {
    val few = List.fill(5)(commit("a/A.scala", "b/B.scala")) ++ List.fill(12)(commit("c/C.scala")) ++ List.fill(8)(commit("a/A.scala"))
    assertEquals(run(few, three).findings, Nil)
  }

  test("a package touched by almost every commit co-changes with everything: the base rate is not coupling") {
    val busy = List(file("core/Core.scala", "core", "Core"), file("b/B.scala", "b", "B"), file("c/C.scala", "c", "C"))
    val cs = List.fill(12)(commit("core/Core.scala", "b/B.scala")) ++ List.fill(10)(commit("core/Core.scala", "c/C.scala"))
    assertEquals(run(cs, busy).findings, Nil)
  }

  test("a package and the only package that uses it are not reported") {
    assertEquals(run(together, three, "a" -> "b").findings, Nil)
    // with a second user the dependency is a shared API, not a private detail
    val facts = three :+ file("d/D.scala", "d", "D")
    assertEquals(run(together, facts, "a" -> "b", "d" -> "b").findings.map(_.key), List("co-change a <-> b"))
  }

  test("a package and its own subpackage are one family") {
    val facts = List(file("a/A.scala", "a", "A"), file("a/sub/S.scala", "a.sub", "S"), file("c/C.scala", "c", "C"))
    val cs = List.fill(10)(commit("a/A.scala", "a/sub/S.scala")) ++ List.fill(12)(commit("c/C.scala"))
    assertEquals(run(cs, facts).findings, Nil)
  }

  test("a namespace over several slices is not a family with them") {
    val facts = List(file("A.scala", "app", "A"), file("x/X.scala", "app.x", "X"), file("y/Y.scala", "app.y", "Y"), file("c/C.scala", "c", "C"))
    val cs = List.fill(10)(commit("A.scala", "x/X.scala")) ++ List.fill(12)(commit("c/C.scala"))
    assertEquals(run(cs, facts).findings.map(_.key), List("co-change app <-> app.x"))
  }

  test("mass commits and commits across many packages are cross-cutting changes, not features") {
    val many = (1 to 40).map(i => s"c/F$i.scala").toList
    val massive = List.fill(10)(ChangeScatter.Commit((many :+ "a/A.scala" :+ "b/B.scala").map(root + _).toSet, 42))
    assertEquals(run(massive ++ List.fill(22)(commit("c/C.scala")), three).findings, Nil)
    val ps = ('a' to 'h').toList.map(c => file(s"$c/X.scala", c.toString, "X"))
    val wide = List.fill(10)(commit(ps.map(_.source.stripPrefix(root))*))
    val r = ChangeScatter.analyze(wide ++ List.fill(12)(commit("a/X.scala")), ps, graphOf(ps), o)
    assertEquals(r.findings, Nil)
    assert(r.note.exists(_.contains("touching more than 6 packages")), r.note)
  }

  test("too little history skips the check with a warning instead of failing") {
    val r = run(together.take(5), three)
    assert(!r.checked && r.findings.isEmpty && r.warnings.exists(_.contains("skipped")), r.warnings)
  }

  test("renames are followed, so moving a file moves its history with it") {
    // newest first: the last commit moves b/B.scala into a; earlier commits changed it as b/B.scala
    val log =
      "\u0000\nR100\tsrc/b/B.scala\tsrc/a/B.scala\n" +
        (1 to 10).map(_ => "\u0000\nM\tsrc/a/A.scala\nM\tsrc/b/B.scala\n").mkString +
        (1 to 12).map(_ => "\u0000\nM\tsrc/c/C.scala\n").mkString
    val parsed = ChangeScatter.parseStatusLog(log)
    assert(parsed.drop(1).take(10).forall(_.files == Set("src/a/A.scala", "src/a/B.scala")), parsed.take(3).toString)
    val facts = List(
      file("a/A.scala", "a", "A"), file("a/B.scala", "a", "B"), file("c/C.scala", "c", "C"), file("b/Other.scala", "b", "Other")
    )
    val commits = parsed.map(c => c.copy(files = c.files.map("/repo/" + _)))
    assertEquals(ChangeScatter.analyze(commits, facts, graphOf(facts), o).findings, Nil)
    // without following renames the same history reads as a and b changing together
    val naive = commits.map(c => c.copy(files = c.files.map(_.replace("/a/B.scala", "/b/B.scala"))))
    val facts2 = List(file("a/A.scala", "a", "A"), file("b/B.scala", "b", "B"), file("c/C.scala", "c", "C"))
    assertEquals(ChangeScatter.analyze(naive, facts2, graphOf(facts2), o).findings.map(_.key), List("co-change a <-> b"))
  }

  test("mass commits still contribute their renames") {
    val log = "\u0000\nR100\tsrc/b/B.scala\tsrc/a/B.scala\n" + (1 to 40).map(i => s"M\tsrc/z/F$i.scala\n").mkString +
      "\u0000\nM\tsrc/b/B.scala\n"
    val parsed = ChangeScatter.parseStatusLog(log)
    assertEquals(parsed.last.files, Set("src/a/B.scala"))
    assertEquals(parsed.head.size, 41)
  }

  test("a file deleted from history gets the package of its directory") {
    val facts = List(file("a/A.scala", "a", "A"), file("b/B.scala", "b", "B"), file("c/C.scala", "c", "C"))
    val cs = List.fill(10)(commit("a/A.scala", "b/Gone.scala")) ++ List.fill(12)(commit("c/C.scala"))
    assertEquals(run(cs, facts).findings.map(_.key), List("co-change a <-> b"))
  }

  // ---------------------------------------------------------------- feature spread

  private val feature = List(
    file("model/Prodotto.scala", "app.model", "Prodotto"),
    file("database/ProdottiDB.scala", "app.database", "ProdottiDB"),
    file("pages/PaginaProdotto.scala", "app.pages", "PaginaProdotto"),
    file("other/Other.scala", "app.other", "Other")
  )
  private val featureCommits =
    List.fill(9)(commit("model/Prodotto.scala", "database/ProdottiDB.scala", "pages/PaginaProdotto.scala")) ++
      List.fill(12)(commit("other/Other.scala"))

  test("a business noun in the names of three layers that change together is a feature spread, with a proposed slice") {
    val r = run(featureCommits, feature)
    val f = r.findings.find(_.kind == "feature-spread").get
    assertEquals(f.key, "feature-spread prodott")
    assertEquals(f.value, 3)
    assert(f.message.contains("Proposed slice `app.prodotto`"), f.message)
    assert(f.message.contains("ProdottiDB.scala") && f.message.contains("Prodotto.scala"), f.message)
    assert(f.message.contains("Done when"), f.message)
  }

  test("technical words are not features, and fewer layers than the minimum is not a spread") {
    val tech = List(
      file("a/UserService.scala", "app.a", "UserService"), file("b/OrderService.scala", "app.b", "OrderService"),
      file("c/PayService.scala", "app.c", "PayService"), file("o/Other.scala", "app.o", "Other")
    )
    val cs = List.fill(10)(commit("a/UserService.scala", "b/OrderService.scala", "c/PayService.scala")) ++ List.fill(12)(commit("o/Other.scala"))
    assertEquals(run(cs, tech).findings.filter(_.kind == "feature-spread"), Nil)
    val two = feature.filterNot(_.source.contains("pages"))
    assertEquals(run(featureCommits.map(c => c.copy(files = c.files.filterNot(_.contains("pages")))), two).findings.filter(_.kind == "feature-spread"), Nil)
  }

  test("a feature whose files do not change together is not a spread, and the noun is listed as unproven when history is short") {
    val apart = List.fill(2)(commit("model/Prodotto.scala")) ++ List.fill(2)(commit("database/ProdottiDB.scala")) ++
      List.fill(2)(commit("pages/PaginaProdotto.scala")) ++ List.fill(12)(commit("other/Other.scala"))
    val r = run(apart, feature)
    assertEquals(r.findings.filter(_.kind == "feature-spread"), Nil)
    assert(r.note.exists(_.contains("Prodotto")), r.note)
  }

  test("renaming the files out of the noun does not hide the co-change: the history follows them") {
    // the files are renamed in the newest commit; the old names carried the noun, the new ones do not
    val log =
      "\u0000\nR100\tsrc/model/Prodotto.scala\tsrc/model/Item.scala\nR100\tsrc/database/ProdottiDB.scala\tsrc/database/ItemDB.scala\n" +
        (1 to 9).map(_ => "\u0000\nM\tsrc/model/Prodotto.scala\nM\tsrc/database/ProdottiDB.scala\nM\tsrc/pages/PaginaProdotto.scala\n").mkString +
        (1 to 12).map(_ => "\u0000\nM\tsrc/other/Other.scala\n").mkString
    val commits = ChangeScatter.parseStatusLog(log).map(c => c.copy(files = c.files.map("/repo/" + _)))
    val renamed = List(
      file("model/Item.scala", "app.model", "Item"), file("database/ItemDB.scala", "app.database", "ItemDB"),
      file("pages/PaginaProdotto.scala", "app.pages", "PaginaProdotto"), file("other/Other.scala", "app.other", "Other")
    )
    // the noun is still in one name, but a spread needs it in three layers: the rename removed it from two
    assertEquals(ChangeScatter.analyze(commits, renamed, graphOf(renamed), o).findings.filter(_.kind == "feature-spread"), Nil)
    // what the rename moved is still seen as co-changing by package, which is the part that cannot be renamed away
    val r = ChangeScatter.analyze(commits, renamed, graphOf(renamed), o)
    assert(r.findings.exists(_.kind == "co-change"), r.findings.map(_.key).toString)
  }

  test("options: off without history, window and thresholds from the options") {
    assertEquals(ChangeScatter.options(List("design")), None)
    val on = ChangeScatter.options(List("history=500 commits", "minCoChange=60", "minCoChangeCommits=12", "maxCommitFiles=10", "featureStopWords=foo; Bar")).get
    assertEquals((on.window, on.minCoChange, on.minCommits, on.maxCommitFiles), ("500 commits", 0.6, 12, 10))
    assert(on.stopWords("foo") && on.stopWords("bar") && on.stopWords("service"))
    assertEquals(GitSignals.windowArgs("500 commits"), List("-n", "500"))
    assertEquals(GitSignals.windowArgs("1 year ago"), List("--since=1 year ago"))
  }

  private val catasto = List(
    file("catasto/Service.scala", "catasto", "Service"), file("catasto/Db.scala", "catasto", "Db"),
    file("catasto/Gml.scala", "catasto", "Gml"), file("database/common.scala", "database", "Common"),
    file("database/GlobalDB.scala", "database", "GlobalDB"), file("c/C.scala", "c", "C")
  )
  private val noise = List.fill(14)(commit("c/C.scala"))

  test("a concentrated pair of files is reported, with the pair first in the list") {
    val cs = List.fill(11)(commit("catasto/Service.scala", "database/common.scala")) ++ List(commit("catasto/Db.scala", "database/GlobalDB.scala")) ++ noise
    val r = run(cs, catasto)
    assertEquals(r.findings.map(_.key), List("co-change catasto <-> database"))
    assert(r.findings.head.message.contains(" 11 commits  /repo/src/catasto/Service.scala  <->  /repo/src/database/common.scala"), r.findings.head.message)
  }

  test("diffuse co-change through a wiring file is not reported, and the note says so") {
    // every feature touches common.scala and one of five catasto files: 10 joint commits, no file pair above 2
    val names = List("Service", "Db", "Gml", "Zip", "Import")
    val facts = names.map(n => file(s"catasto/$n.scala", "catasto", n)) ++ List(file("database/common.scala", "database", "Common"), file("c/C.scala", "c", "C"))
    val cs = (0 until 10).toList.map(i => commit(s"catasto/${names(i % 5)}.scala", "database/common.scala")) ++ noise
    val r = run(cs, facts)
    assertEquals(r.findings, Nil)
    assert(r.note.exists(n => n.contains("1 package pairs change together only diffusely") && n.contains("catasto <-> database (10 commits, best file pair 2)")), r.note)
  }

  test("a file pair needs a share of the joint commits as well as the minimum count") {
    // 4 commits for one pair; the other joint commits rotate over six file pairs, none reaching 4
    def history(others: Int) =
      List.fill(4)(commit("catasto/Service.scala", "database/common.scala")) ++
        (0 until others).toList.map { i =>
          val cat = if i % 2 == 0 then "catasto/Gml.scala" else "catasto/Db.scala"
          commit(cat, List("database/GlobalDB.scala", "database/Other.scala", "database/Other2.scala")(i % 3))
        } ++ List.fill(14)(commit("c/C.scala"))
    val facts = catasto ++ List(file("database/Other.scala", "database", "Other"), file("database/Other2.scala", "database", "Other2"))
    // 4 of 16 joint commits is 25%: concentrated; 4 of 17 is 24%: diffuse
    assertEquals(run(history(12), facts).findings.map(_.key), List("co-change catasto <-> database"))
    assertEquals(run(history(13), facts).findings, Nil)
    // and the count: no pair of files reaches 4 commits however many commits the packages share
    val tiny = List.fill(3)(commit("catasto/Service.scala", "database/common.scala")) ++ List.fill(3)(commit("catasto/Gml.scala", "database/GlobalDB.scala")) ++
      List.fill(3)(commit("catasto/Db.scala", "database/Other.scala")) ++ noise
    assertEquals(run(tiny, facts).findings, Nil)
  }

  test("moving or renaming a file does not hide a concentrated pair: its history follows it") {
    val log =
      "\u0000\nR100\tsrc/database/common.scala\tsrc/database/Plumbing.scala\n" +
        (1 to 10).map(_ => "\u0000\nM\tsrc/catasto/Service.scala\nM\tsrc/database/common.scala\n").mkString +
        (1 to 14).map(_ => "\u0000\nM\tsrc/c/C.scala\n").mkString
    val commits = ChangeScatter.parseStatusLog(log).map(c => c.copy(files = c.files.map("/repo/" + _)))
    val facts = catasto.filterNot(_.source.endsWith("common.scala")) :+ file("database/Plumbing.scala", "database", "Plumbing")
    val r = ChangeScatter.analyze(commits, facts, graphOf(facts), o)
    assertEquals(r.findings.map(_.key), List("co-change catasto <-> database"))
    assert(r.findings.head.message.contains("Plumbing.scala"), r.findings.head.message)
  }

  test("a file moved and rewritten (not a git rename) keeps its history under its new package, if its name is unique") {
    // the old database/Db.scala is gone; catasto/Db.scala is the one current file of that name
    val facts = List(file("catasto/Service.scala", "catasto", "Service"), file("catasto/Db.scala", "catasto", "Db"), file("c/C.scala", "c", "C"), file("database/GlobalDB.scala", "database", "GlobalDB"))
    val cs = List.fill(11)(commit("catasto/Service.scala", "database/Db.scala")) ++ noise
    assertEquals(run(cs, facts).findings, Nil)
    // two current files of that name: which one it became is unknown, the directory decides
    val two = facts :+ file("other/Db.scala", "other", "Db")
    assertEquals(run(cs, two).findings.map(_.key), List("co-change catasto <-> database"))
  }

  test("a wiring file that changes with many packages does not make a pair concentrated") {
    // Parser (in sian) changed with GlobalDB 9 times; GlobalDB also changes with nine other packages (ten with sian): it is the aggregator
    val others = (1 to 9).map(i => file(s"p$i/P.scala", s"p$i", "P")).toList
    val facts = List(file("sian/Parser.scala", "sian", "Parser"), file("database/GlobalDB.scala", "database", "GlobalDB"), file("c/C.scala", "c", "C")) ++ others
    val wiringCommits = others.indices.toList.map(i => commit(s"p${i + 1}/P.scala", "database/GlobalDB.scala"))
    val cs = List.fill(9)(commit("sian/Parser.scala", "database/GlobalDB.scala")) ++ wiringCommits ++ noise
    val r = run(cs, facts)
    assertEquals(r.findings, Nil)
    assert(r.note.exists(_.contains("diffusely")), r.note)
    // the same history with GlobalDB changing only with the parser is a concentrated pair
    assertEquals(run(List.fill(9)(commit("sian/Parser.scala", "database/GlobalDB.scala")) ++ noise, facts).findings.map(_.key), List("co-change database <-> sian"))
    assertEquals(ChangeScatter.options(List("history=500 commits", "wiringPackages=3")).get.wiringPackages, 3)
  }

  test("the concentration limits are options") {
    val on = ChangeScatter.options(List("history=500 commits", "minPairCommits=6", "minPairShare=40")).get
    assertEquals((on.minPairCommits, on.minPairShare), (6, 0.4))
    val d = ChangeScatter.options(List("history=500 commits")).get
    assertEquals((d.minPairCommits, d.minPairShare), (4, 0.25))
  }

  test("a stem meets singular, plural and trailing vowels") {
    assertEquals(ChangeScatter.nouns("ProdottiDB", ChangeScatter.defaultStopWords).map(_._1), List("prodott"))
    assertEquals(ChangeScatter.nouns("PaginaProdotto", ChangeScatter.defaultStopWords).map(_._1), List("pagin", "prodott"))
    assertEquals(ChangeScatter.nouns("UserService", ChangeScatter.defaultStopWords).map(_._1), List("user"))
  }
