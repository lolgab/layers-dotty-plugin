package layers.plugin

import munit.FunSuite

class ClassMetricsTests extends FunSuite:

  private val ok = MethodFact("F.scala", 1, "a.B.m", 1, 0, 3, 3, 1, 0)
  // cluster caps at 1: these tests are about sums being judged; ClusterTests covers the caps
  private val t = Thresholds(maxClusterLines = 1, maxClusterCyclomatic = 1)

  test("measurements within limits produce no findings") {
    assertEquals(ClassMetrics.findings(List(ok), Nil, t), Nil)
  }

  test("every exceeded limit produces its own finding with value and fix") {
    val bad = ok.copy(cyclomatic = 11, cognitive = 16, lines = 61, size = 61, params = 6, nesting = 5)
    val fs = ClassMetrics.findings(List(bad), Nil, t)
    assertEquals(fs.map(_.key).toSet, Set("cyclomatic a.B.m", "cognitive a.B.m", "lines a.B.m", "params a.B.m", "nesting a.B.m"))
    val cyc = fs.find(_.key.startsWith("cyclomatic")).get
    assertEquals(cyc.value, 11)
    assert(cyc.message.contains("limit 10"), cyc.message)
    assert(cyc.message.contains("Fix with one of"), cyc.message)
  }

  test("limit itself is allowed") {
    val edge = ok.copy(cyclomatic = 10, cognitive = 15, lines = 60, size = 60, params = 5, nesting = 4)
    assertEquals(ClassMetrics.findings(List(edge), Nil, t), Nil)
  }

  test("overloads are merged keeping the worst") {
    val fs = ClassMetrics.findings(List(ok.copy(cyclomatic = 11), ok.copy(cyclomatic = 15, line = 9)), Nil, t)
    assertEquals(fs.map(_.value), List(15))
  }

  private def cm(name: String, types: List[String] = Nil, calls: List[String] = Nil, fields: List[String] = Nil) =
    ClassMethod(name, types, calls, fields)

  private def cls(methods: List[ClassMethod], exports: Int = 0) = ClassFact("F.scala", 3, "a.C", methods, exports)

  test("methods on different domain types are separate responsibilities, reported with their topics") {
    val c = cls(
      List(
        cm("addCat", List("m.Cat")), cm("cats", List("m.Cat")),
        cm("addDog", List("m.Dog")), cm("dogs", List("m.Dog")),
        cm("addCow", List("m.Cow")), cm("cows", List("m.Cow")),
        cm("addPig", List("m.Pig")), cm("pigs", List("m.Pig")),
        cm("addHen", List("m.Hen")), cm("hens", List("m.Hen"))
      )
    )
    val fs = ClassMetrics.findings(Nil, List(c), t)
    assertEquals(fs.map(_.key), List("cohesion a.C"))
    assertEquals(fs.head.value, 5)
    assert(fs.head.message.contains("[Cat] addCat, cats"), fs.head.message)
    assert(ClassMetrics.findings(Nil, List(c), t.copy(maxResponsibilities = 5)).isEmpty)
  }

  test("calls, shared fields and shared types tie methods into one responsibility") {
    val c = cls(
      List(
        cm("a", List("m.X")), cm("b", List("m.X")), // same type
        cm("c", fields = List("conn")), cm("d", fields = List("conn")), // same field
        cm("e", calls = List("f")), cm("f"), // one calls the other
        cm("g", List("m.Y")), cm("h", List("m.Z")), cm("i", List("m.W")), cm("j", List("m.V"))
      )
    )
    val rs = ClassMetrics.responsibilities(c)
    assert(rs.exists(_.methods == List("a", "b")), rs.toString)
    assert(rs.exists(_.methods == List("c", "d")), rs.toString)
    assert(rs.exists(_.methods == List("e", "f")), rs.toString)
  }

  test("a type, field or helper used by most methods is plumbing and ties nothing together") {
    // every method goes through `transact` and takes a `Ctx`: that must not make the class look cohesive
    val ms = List("cats", "dogs", "cows", "pigs", "hens", "bees", "owls", "rats").map(n => cm(n, List("m.Ctx", "m." + n), List("transact"), List("db")))
    val c = cls(cm("transact", Nil, Nil, List("db")) :: ms)
    val fs = ClassMetrics.findings(Nil, List(c), t)
    assertEquals(fs.map(_.key), List("cohesion a.C"), fs.toString)
    assertEquals(ClassMetrics.countResponsibilities(ClassMetrics.responsibilities(c)), 8)
  }

  test("a mutable field is shared state even when most methods use it") {
    val ms = List("a", "b", "c", "d", "e", "f", "g", "h").map(n => cm(n, Nil, Nil, List("count!")))
    assertEquals(ClassMetrics.countResponsibilities(ClassMetrics.responsibilities(cls(ms))), 1)
  }

  test("typeless one-off methods count together as one responsibility") {
    val c = cls(List(cm("a"), cm("b"), cm("c"), cm("d"), cm("e", List("m.X")), cm("f", List("m.X"))))
    assertEquals(ClassMetrics.countResponsibilities(ClassMetrics.responsibilities(c)), 2)
  }

  test("tiny classes are not judged") {
    assertEquals(ClassMetrics.findings(Nil, List(cls(List(cm("a", List("m.A")), cm("b", List("m.B")), cm("c", List("m.C"))))), t.copy(maxResponsibilities = 1)), Nil)
  }

  test("a class that mostly re-exports is a facade") {
    val c = cls(List(cm("own", List("m.A")), cm("mine", List("m.A"))), exports = 12)
    assertEquals(ClassMetrics.findings(Nil, List(c), t).map(_.key), List("facade a.C"))
    assertEquals(ClassMetrics.findings(Nil, List(c.copy(exports = 7)), t), Nil)
    assertEquals(ClassMetrics.findings(Nil, List(cls(List.fill(20)(cm("m", List("m.A"))), exports = 9)), t), Nil)
  }

  private def fact(sym: String, cog: Int = 0, lines: Int = 3, priv: Boolean = false, calls: List[String] = Nil, cyc: Int = 1, params: Int = 1, nest: Int = 0, file: String = "F.scala") =
    MethodFact(file, 1, sym, cyc, cog, lines, lines, params, nest, priv, calls)

  test("single-use private helpers are measured with their caller, so splitting a method does not help") {
    val ms = List(
      fact("a.S.run", cog = 6, lines = 20, calls = List("a.S.stepOne", "a.S.stepTwo")),
      fact("a.S.stepOne", cog = 6, lines = 20, priv = true),
      fact("a.S.stepTwo", cog = 6, lines = 25, priv = true)
    )
    val fs = ClassMetrics.findings(ms, Nil, t)
    assertEquals(fs.map(_.key).sorted, List("cognitive a.S.run", "lines a.S.run"), fs.toString)
    assertEquals(fs.find(_.key.startsWith("cognitive")).get.value, 18)
    assertEquals(fs.find(_.key.startsWith("lines")).get.value, 65)
    assert(fs.head.message.contains("single-use helper"), fs.head.message)
    assert(!fs.exists(_.key.contains("stepOne")), fs.toString)
  }

  test("a private helper used by two methods is real reuse and stays separate") {
    val ms = List(
      fact("a.S.x", cog = 9, calls = List("a.S.shared")),
      fact("a.S.y", cog = 9, calls = List("a.S.shared")),
      fact("a.S.shared", cog = 9, priv = true)
    )
    assertEquals(ClassMetrics.findings(ms, Nil, t), Nil)
  }

  test("a public helper only its same-file caller uses is absorbed: widening visibility is not a design change") {
    val ms = List(fact("a.S.run", cog = 9, calls = List("a.S.help")), fact("a.S.help", cog = 9, priv = false))
    assertEquals(ClassMetrics.findings(ms, Nil, t).map(_.key), List("cognitive a.S.run"))
  }

  test("a public helper in another file with one caller is absorbed: moving code to another file is not a design change") {
    val ms = List(
      fact("a.S.run", cog = 9, calls = List("b.Help.part")),
      fact("b.Help.part", cog = 9, file = "Help.scala")
    )
    assertEquals(ClassMetrics.findings(ms, Nil, t).map(_.key), List("cognitive a.S.run"))
  }

  test("a public method used by two callers is shared and stays separate") {
    val ms = List(
      fact("a.S.x", cog = 9, calls = List("a.S.help")), fact("a.S.y", cog = 9, calls = List("a.S.help")),
      fact("a.S.help", cog = 9)
    )
    assertEquals(ClassMetrics.findings(ms, Nil, t), Nil)
  }

  test("a helper's own parameter count is still reported") {
    val ms = List(fact("a.S.run", calls = List("a.S.help")), fact("a.S.help", priv = true, params = 7))
    assertEquals(ClassMetrics.findings(ms, Nil, t).map(_.key), List("params a.S.help"))
  }

  test("helpers calling each other in a cycle do not hang the analysis") {
    val ms = List(
      fact("a.S.run", cog = 20, calls = List("a.S.p")),
      fact("a.S.p", priv = true, cog = 1, calls = List("a.S.q")),
      fact("a.S.q", priv = true, cog = 1, calls = List("a.S.p"))
    )
    assertEquals(ClassMetrics.findings(ms, Nil, t).map(_.key), List("cognitive a.S.run"))
  }

  test("a file over the limit is a finding keyed by its name") {
    val big = FileFacts("/x/y/Big.scala", Set("a"), Nil, lines = 1600)
    val small = FileFacts("/x/y/Small.scala", Set("a"), Nil, lines = 1500)
    val fs = ClassMetrics.fileFindings(List(big, small), t)
    assertEquals(fs.map(_.key), List("filesize Big.scala"))
    assertEquals(fs.head.value, 1600)
    assertEquals(ClassMetrics.fileFindings(List(big), t.copy(maxFileLines = 2000)), Nil)
  }

  test("thresholds come from options") {
    val th = Thresholds.fromOptions(List("design", "maxCyclomatic=3", "maxResponsibilities=x", "maxParams=0"))
    assertEquals(th, Thresholds(maxCyclomatic = 3))
    assertEquals(Thresholds.fromOptions(List("maxResponsibilities=9")).maxResponsibilities, 9)
  }

  test("method and class facts survive the codec") {
    val f = FileFacts(
      "/x/A.scala", Set("a"), Nil, Map.empty,
      List(MethodFact("/x/A.scala", 4, "a.B.m", 3, 5, 12, 12, 2, 1, priv = true, calls = List("a.B.h"))),
      List(ClassFact("/x/A.scala", 2, "a.B", List(ClassMethod("m", List("t.X", "t.Y"), List("h"), List("f")), ClassMethod("h", Nil, Nil, Nil)), 3))
    )
    assertEquals(FileFacts.decode(FileFacts.encode(f)), Some(f))
  }
