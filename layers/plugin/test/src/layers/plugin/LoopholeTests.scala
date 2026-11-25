package layers.plugin

import java.nio.file.Files

import munit.FunSuite

/** Closing the ways to satisfy the design limits without improving the design: moving single-use helpers elsewhere,
  * writing branches as combinators, hiding methods in utility objects, putting code where it is not measured.
  */
class LoopholeTests extends FunSuite:

  private def pluginJarPath: String =
    Option(System.getProperty("layers.plugin.jar")).getOrElse(sys.error("layers.plugin.jar system property not set."))

  private def compile(sources: List[(String, String)], opts: String*) =
    CompilerPluginTestHelper.compile(sources, pluginJarPath, "design" :: opts.toList)

  private def design(source: String, opts: String*) =
    compile(List("Layer.scala" -> "package p\nobject layer\n", "Src.scala" -> source), opts*)

  private def fact(sym: String, cog: Int = 0, lines: Int = 3, calls: List[String] = Nil, file: String = "F.scala", entry: Boolean = false, priv: Boolean = false, cyc: Int = 1, pkg: String = "") =
    MethodFact(file, 1, sym, cyc, cog, lines, lines, 1, 0, priv, calls, entry, if pkg.isEmpty then sym.takeWhile(_ != '.') else pkg)

  // cluster caps at 1: these tests are about sums being judged; ClusterTests covers the caps
  private val t = Thresholds(maxClusterLines = 1, maxClusterCyclomatic = 1)

  // 1. single-use helpers, wherever they live

  test("a single-use helper moved to another file, object or visibility of the same package is still measured with its caller") {
    val branch = (1 to 4).map(i => s"    if x == $i then $i else").mkString("\n")
    val r = compile(
      List(
        "Layer.scala" -> "package p\nobject layer\n",
        "Run.scala" -> "package p\nclass Run:\n  def run(x: Int): Int = Parts.one(x) + Parts.two(x)\n",
        "Parts.scala" -> s"package p\nobject Parts:\n  def one(x: Int): Int =\n$branch\n    0\n  def two(x: Int): Int =\n$branch\n    0\n"
      ),
      "maxCyclomatic=6", "maxClusterCyclomatic=1"
    )
    assert(r.errorMessages.contains("Method `p.Run.run`") && r.errorMessages.contains("single-use helper"), r.errorMessages)
    assert(!r.errorMessages.contains("Method `p.Parts$.one`"), r.errorMessages)
  }

  test("a single-use helper in another package is a module boundary and is measured on its own") {
    val branch = (1 to 4).map(i => s"    if x == $i then $i else").mkString("\n")
    val r = compile(
      List(
        "LayerP.scala" -> "package p\n@layers.dependsOnPackages(\"q\")\nobject layer\n",
        "LayerQ.scala" -> "package q\nobject layer\n",
        "Run.scala" -> "package p\nclass Run:\n  def run(x: Int): Int = q.Parts.one(x) + q.Parts.two(x)\n",
        "Parts.scala" -> s"package q\nobject Parts:\n  def one(x: Int): Int =\n$branch\n    0\n  def two(x: Int): Int =\n$branch\n    0\n"
      ),
      "maxCyclomatic=6"
    )
    assert(!r.hasErrors, r.errorMessages)
    val ms = List(
      fact("a.S.run", cog = 9, calls = List("b.H.part")), fact("b.H.part", cog = 9, file = "H.scala")
    )
    assertEquals(ClassMetrics.findings(ms, Nil, t), Nil)
    assertEquals(ClassMetrics.findings(ms.map(_.copy(pkg = "a")), Nil, t).map(_.key), List("cognitive a.S.run"))
  }

  test("every finding of a declaration is visible, not only the first one at its position") {
    val r = design(
      """package p
class Calc:
  def f(x: Int): Int =
    if x > 1 then
      if x > 2 then
        if x > 3 then 1 else 2
      else 3
    else 4
""",
      "maxCyclomatic=2", "maxNesting=1", "maxCognitive=2"
    )
    for what <- List("independent paths", "too hard to follow", "nested too deeply") do
      assert(r.errorMessages.contains(what), s"$what missing: ${r.errorMessages}")
  }

  test("a helper with two distinct callers anywhere is real reuse") {
    val ms = List(
      fact("a.X.x", cog = 9, calls = List("b.H.part")), fact("c.Y.y", cog = 9, calls = List("b.H.part")),
      fact("b.H.part", cog = 9, file = "H.scala")
    )
    assertEquals(ClassMetrics.findings(ms, Nil, t), Nil)
  }

  test("several calls from one method are one caller") {
    val ms = List(fact("a.S.run", cog = 9, calls = List("a.H.part", "a.H.part")), fact("a.H.part", cog = 9, file = "H.scala"))
    assertEquals(ClassMetrics.findings(ms, Nil, t).map(_.key), List("cognitive a.S.run"))
  }

  test("a method that overrides an inherited one is called through its parent and is never a helper") {
    val ms = List(fact("a.S.run", cog = 9, calls = List("a.S.part")), fact("a.S.part", cog = 9, entry = true))
    assertEquals(ClassMetrics.findings(ms, Nil, t), Nil)
  }

  test("methods only calling each other, with no outside caller, are still measured") {
    val ms = List(
      fact("a.S.p", cog = 20, calls = List("a.S.q")), fact("a.S.q", cog = 1, calls = List("a.S.p"))
    )
    assert(ClassMetrics.findings(ms, Nil, t).nonEmpty)
  }

  test("a chain of single-use methods across files of a package adds up at its head and says what each part contributes") {
    val ms = List(
      fact("a.Route.get", cog = 5, calls = List("a.Service.load")),
      fact("a.Service.load", cog = 6, file = "Service.scala", calls = List("a.Repo.query")),
      fact("a.Repo.query", cog = 7, file = "Repo.scala")
    )
    val fs = ClassMetrics.findings(ms, Nil, t)
    assertEquals(fs.map(_.key), List("cognitive a.Route.get"))
    assertEquals(fs.head.value, 18)
    val msg = fs.head.message
    assert(msg.contains("Where the cognitive complexity comes from"), msg)
    assert(msg.contains("a.Service.load (Service.scala:1): +6") && msg.contains("a.Repo.query (Repo.scala:1): +7"), msg)
    assert(msg.contains("a.Route.get itself (F.scala:1): 5"), msg)
    assert(msg.indexOf("a.Repo.query") < msg.indexOf("a.Service.load"), "largest first: " + msg)
  }

  test("a helper already over a limit by itself is reported by itself, not again in every caller above it") {
    val ms = List(
      fact("a.Main.main", cog = 2, calls = List("a.Api.server")),
      fact("a.Api.server", cog = 3, file = "Api.scala", calls = List("a.Page.big", "a.Page.small")),
      fact("a.Page.big", cog = 40, file = "Page.scala"),
      fact("a.Page.small", cog = 4, file = "Page.scala")
    )
    val fs = ClassMetrics.findings(ms, Nil, t)
    assertEquals(fs.map(_.key), List("cognitive a.Page.big"))
    // but pieces that each pass add up where they are used
    val split = List(
      fact("a.Main.main", cog = 2, calls = List("a.Api.server")),
      fact("a.Api.server", cog = 6, file = "Api.scala", calls = List("a.Page.x", "a.Page.y")),
      fact("a.Page.x", cog = 6, file = "Page.scala"),
      fact("a.Page.y", cog = 6, file = "Page.scala")
    )
    assertEquals(ClassMetrics.findings(split, Nil, t).map(_.key), List("cognitive a.Api.server"))
  }

  test("a dispatcher calling many single-use methods is not summed, each slice is measured alone") {
    val slices = (1 to 9).map(i => s"a.H.h$i").toList
    val ms = fact("a.R.routes", cog = 1, calls = slices) :: slices.map(h => fact(h, cog = 4, file = "H.scala"))
    assertEquals(ClassMetrics.findings(ms, Nil, t), Nil)
    // up to maxHelpers it is a method that was split
    assertEquals(ClassMetrics.findings(ms.take(9), Nil, t.copy(maxHelpers = 9)).map(_.key), List("cognitive a.R.routes"))
    assertEquals(Thresholds.fromOptions(List("maxHelpers=3")).maxHelpers, 3)
  }

  // 2. branches written as combinators

  private def metric(kind: String, source: String): Int =
    val r = design(source, "maxCyclomatic=1", "maxCognitive=1", "maxNesting=1", "maxMethodLines=1000")
    val pattern = kind match
      case "cyclomatic" => "\\(cyclomatic complexity\\): (\\d+) \\(limit"
      case _            => "\\(cognitive complexity\\): (\\d+) \\(limit"
    pattern.r.findFirstMatchIn(r.errorMessages).map(_.group(1).toInt).getOrElse(0)

  private def body(expr: String, prelude: String = "") =
    s"package p\nobject M:\n  def f(o: Option[Int], e: Either[String, Int], x: Int): Int =\n$prelude    $expr\n"

  test("a match on an Option scores the same as the fold that replaces it") {
    val viaMatch = body("o match\n      case Some(v) => if v > 1 then v else 1\n      case None => 0")
    val viaFold = body("o.fold(0)(v => if v > 1 then v else 1)")
    for kind <- List("cyclomatic", "cognitive") do
      assert(metric(kind, viaFold) >= metric(kind, viaMatch), s"$kind: fold ${metric(kind, viaFold)} < match ${metric(kind, viaMatch)}")
    assert(metric("cyclomatic", viaFold) == 3, viaFold)
  }

  test("a match on an Either scores the same as Either.fold") {
    val viaMatch = body("e match\n      case Left(m) => m.length\n      case Right(v) => v")
    val viaFold = body("e.fold(m => m.length, v => v)")
    assertEquals(metric("cyclomatic", viaFold), metric("cyclomatic", viaMatch))
    assertEquals(metric("cognitive", viaFold), metric("cognitive", viaMatch))
  }

  test("getOrElse with a constant costs nothing, with a computation it is a branch") {
    assertEquals(metric("cyclomatic", body("o.map(_ + 1).getOrElse(0)")), 0)
    assertEquals(metric("cyclomatic", body("o.getOrElse(x)")), 0)
    assertEquals(metric("cyclomatic", body("o.getOrElse(Seq(1, 2).size)")), 2)
    assertEquals(metric("cyclomatic", body("o.getOrElse(throw new IllegalStateException(\"none\"))")), 2)
    assertEquals(metric("cyclomatic", body("o.getOrElse(if x > 1 then x else -x)")), 3)
  }

  test("orElse chains, Option.when and recover are decisions; map, flatMap and predicates are not") {
    assertEquals(metric("cyclomatic", body("o.orElse(Some(1)).orElse(Some(2)).get")), 3)
    // a chain is one step to read, two separate chains are two
    assertEquals(metric("cognitive", body("List(o.orElse(Some(1)).orElse(Some(2)), o.orElse(Some(3))).size")), 2)
    assertEquals(metric("cyclomatic", body("Option.when(x > 1)(x).getOrElse(0)")), 2)
    assertEquals(metric("cyclomatic", body("scala.util.Try(x).recover { case _: Exception => 0 }.get")), 2)
    // predicates normalise values, they do not choose between two courses of action
    assertEquals(metric("cyclomatic", body("if o.exists(_ > 1) then 1 else 0")), 2)
    assertEquals(metric("cyclomatic", body("o.filter(_ > 1).map(_ + 1).getOrElse(0)")), 0)
    assertEquals(metric("cyclomatic", body("o.map(_ + 1).flatMap(Some(_)).get")), 0)
    // a collection's exists is an iteration, not a decision
    assertEquals(metric("cyclomatic", body("if List(1, 2).exists(_ > x) then 1 else 0")), 2)
  }

  // 3. objects and cohesion

  private val shapes =
    """package p
case class Cat(n: String)
case class Dog(n: String)
case class Cow(n: String)
case class Hen(n: String)
case class Pig(n: String)
"""

  test("an object named FormUtil with unrelated definitions is a cohesion finding") {
    val r = design(
      shapes + """
object FormUtil:
  def cat(c: Cat): Int = 1
  def cats(c: Cat): Int = 2
  def dog(c: Dog): Int = 1
  def dogs(c: Dog): Int = 2
  def cow(c: Cow): Int = 1
  def cows(c: Cow): Int = 2
  def hen(c: Hen): Int = 1
  def hens(c: Hen): Int = 2
  def pig(c: Pig): Int = 1
  def pigs(c: Pig): Int = 2
"""
    )
    assert(r.errorMessages.contains("Class `p.FormUtil` has 5 responsibilities"), r.errorMessages)
  }

  test("definitions of a package object are checked for cohesion") {
    val r = compile(
      List(
        "Layer.scala" -> "package q\nobject layer\n",
        "Pkg.scala" -> (shapes.replace("package p", "package q") + """
def cat(c: Cat): Int = 1
def cats(c: Cat): Int = 2
def dog(c: Dog): Int = 1
def dogs(c: Dog): Int = 2
def cow(c: Cow): Int = 1
def cows(c: Cow): Int = 2
def hen(c: Hen): Int = 1
def hens(c: Hen): Int = 2
def pig(c: Pig): Int = 1
def pigs(c: Pig): Int = 2
""")
      )
    )
    assert(r.errorMessages.contains("responsibilities (limit 4)"), r.errorMessages)
  }

  test("a utility object of unrelated functions on standard types is not one harmless remainder") {
    val r = design(
      """package p
object FormUtil:
  def escape(s: String): String = s.trim
  def money(d: BigDecimal): String = d.toString
  def date(l: Long): String = l.toString
  def pad(s: String, n: Int): String = s + n
  def slug(s: String): String = s.toLowerCase
  def percent(d: Double): String = d.toString
"""
    )
    assert(r.errorMessages.contains("Class `p.FormUtil` has 6 responsibilities"), r.errorMessages)
    assert(r.errorMessages.contains("shared utility"), r.errorMessages)
    // the same functions in a class with another name keep the lenient rule for typeless remainders
    val plain = design(r.errorMessages.take(0) + """package p
object Strings:
  def escape(s: String): String = s.trim
  def money(d: BigDecimal): String = d.toString
  def date(l: Long): String = l.toString
  def pad(s: String, n: Int): String = s + n
  def slug(s: String): String = s.toLowerCase
  def percent(d: Double): String = d.toString
""")
    assert(!plain.hasErrors, plain.errorMessages)
  }

  test("an object used from five other packages is judged as a grab-bag, whatever its name") {
    val c = ClassFact("F.scala", 1, "a.Registry", List("a", "b", "c", "d", "e").map(n => ClassMethod(n, Nil, Nil, Nil)))
    assertEquals(ClassMetrics.findings(Nil, List(c), t), Nil)
    assertEquals(ClassMetrics.findings(Nil, List(c), t, Map("a.Registry" -> 5)).map(_.key), List("cohesion a.Registry"))
    assertEquals(ClassMetrics.findings(Nil, List(c), t, Map("a.Registry" -> 4)), Nil)
    val deps = (1 to 5).toList.map(i => ClassDep("x", 1, s"p$i.User", "a.Registry", s"p$i", "a", 1))
    val obj = TypeDecl("x", 1, "a.Registry$", "a", "object", false, false, false, Nil)
    assertEquals(ClassMetrics.fanIn(List(FileFacts("x", Set("a"), Nil, classDeps = deps, typeDecls = List(obj)))), Map("a.Registry" -> 5))
    // a class or trait (a domain entity, with or without a companion) used everywhere is not a bucket
    val entity = obj.copy(sym = "a.Registry", kind = "class")
    assertEquals(ClassMetrics.fanIn(List(FileFacts("x", Set("a"), Nil, classDeps = deps, typeDecls = List(entity, obj)))), Map.empty[String, Int])
  }

  // 4. code outside defs

  test("statements in a class body are measured like a method") {
    val stmts = (1 to 5).map(i => s"  if x > $i then println($i)").mkString("\n")
    val r = design(s"package p\nclass Boot(x: Int):\n$stmts\n", "maxCyclomatic=3")
    assert(r.errorMessages.contains("Method `p.Boot.<init>`") && r.errorMessages.contains("independent paths"), r.errorMessages)
    val o = design(s"package p\nobject Boot2:\n  val x = 3\n$stmts\n", "maxCyclomatic=3")
    assert(o.errorMessages.contains("Method `p.Boot2$.<init>`"), o.errorMessages)
  }

  test("a lazy val and a given initialiser are measured like a method") {
    val table = (1 to 6).map(i => s"if x > $i then $i else").mkString(" ")
    val lz = design(s"package p\nclass L(x: Int):\n  lazy val v: Int = $table 0\n", "maxCyclomatic=3")
    assert(lz.errorMessages.contains("`p.L.v`"), lz.errorMessages)
    val gv = design(s"package p\nclass G(x: Int):\n  given g: Ordering[Int] = { val k = $table 0; Ordering.Int }\n", "maxCyclomatic=3")
    assert(gv.errorMessages.contains("`p.G.g`"), gv.errorMessages)
    val gw = design(
      s"package p\nclass W(x: Int):\n  given Ordering[String] with\n    def compare(a: String, b: String): Int = $table 0\n",
      "maxCyclomatic=3"
    )
    assert(gw.errorMessages.contains("independent paths"), gw.errorMessages)
  }

  test("a single-use val is absorbed into the method that reads it, so a table moved into a val still counts") {
    val table = (1 to 4).map(i => s"(y: Int) => if y > $i then $i else 0").mkString(", ")
    val r = design(
      s"package p\nobject T:\n  private val rules = List($table)\n  def run(x: Int): Int = if x > 0 then rules.map(_(x)).sum else 0\n",
      "maxCyclomatic=5", "maxClusterCyclomatic=1"
    )
    assert(r.errorMessages.contains("Method `p.T$.run`") && r.errorMessages.contains("single-use helper"), r.errorMessages)
  }

  // 5. report

  test("report writes every measured value, with limits and the helpers merged into a method") {
    val dir = Files.createTempDirectory("layers-report")
    val file = dir.resolve("sub").resolve("report.tsv")
    val r = design(
      """package p
class C:
  def run(x: Int): Int = help(x) + (if x > 1 then 1 else 0)
  private def help(x: Int): Int = if x > 2 then 2 else 0
  def other(a: Int, b: Int): Int = a + b
""",
      "report=" + file
    )
    assert(!r.hasErrors, r.errorMessages)
    val lines = Files.readAllLines(file).toArray.toList.map(_.toString)
    assert(lines.head.startsWith("# kind\tsymbol\tvalue\tlimit"), lines.toString)
    def row(kind: String, sym: String) = lines.find(_.startsWith(s"$kind\t$sym\t")).map(_.split('\t').toList)
    val cyc = row("cyclomatic", "p.C.run").get
    assertEquals(cyc.slice(2, 4), List("3", "10"))
    assert(cyc(5).contains("own=2") && cyc(5).contains("merged-helpers=1[help=1]"), cyc.toString)
    assert(row("cyclomatic", "p.C.help").isEmpty, "absorbed helpers are not reported separately")
    assertEquals(row("params", "p.C.other").get.slice(2, 4), List("2", "5"))
    assert(row("lines", "p.C.other").isDefined && row("nesting", "p.C.other").isDefined && row("cognitive", "p.C.other").isDefined)
    assertEquals(row("methods", "p.C").get(2), "3")
    assert(lines.exists(_.startsWith("filelines\tSrc.scala\t")), lines.toString)
    assertEquals(lines.tail, lines.tail.sorted.sortBy(_.split('\t')(0)), "stable order")
  }

  test("a helper's facts survive the codec with the entry flag") {
    val f = FileFacts("/x/A.scala", Set("a"), Nil, Map.empty, List(
      MethodFact("/x/A.scala", 4, "a.B.m", 3, 5, 12, 12, 2, 1, priv = true, calls = List("a.B.h"), entry = true),
      MethodFact("/x/A.scala", 5, "a.B.n", 3, 5, 12, 12, 2, 1)
    ))
    assertEquals(FileFacts.decode(FileFacts.encode(f)), Some(f))
  }
