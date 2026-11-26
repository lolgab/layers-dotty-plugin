package layers.plugin

import munit.FunSuite

class DesignGraphTests extends FunSuite:

  private def pluginJarPath: String =
    Option(System.getProperty("layers.plugin.jar")).getOrElse(
      sys.error("layers.plugin.jar system property not set. Tests must run with forkArgs from build.")
    )

  private def layersJarPath: String =
    Option(System.getProperty("layers.jar")).getOrElse(
      sys.error("layers.jar system property not set. Tests must run with forkArgs from build.")
    )

  private val cyclic = List(
    "Invoice.scala" -> """package billing
class Invoice(val order: orders.Order):
  def total: Int = order.lines.size
""",
    "Order.scala" -> """package orders
class Order(val lines: List[String]):
  def invoice: billing.Invoice = new billing.Invoice(this)
"""
  )

  test("package cycle fails compilation with actionable message") {
    val result = CompilerPluginTestHelper.compile(cyclic, pluginJarPath, "design")
    assert(result.hasErrors, "expected failure")
    val msg = result.errorMessages
    assert(msg.contains("Cyclic dependency between packages: billing, orders"), msg)
    assert(msg.contains("Invoice.scala") || msg.contains("Order.scala"), msg)
    assert(msg.contains("Dependency inversion"), msg)
  }

  test("design checks are off without the option") {
    val result = CompilerPluginTestHelper.compile(cyclic, pluginJarPath)
    assert(!result.errorMessages.contains("Cyclic dependency"), result.errorMessages)
  }

  test("acyclic packages compile") {
    val sources = List(
      "OrdersLayer.scala" -> """package orders
object layer
""",
      "BillingLayer.scala" -> """package billing
@layers.dependsOnPackages("orders")
object layer
""",
      "Order.scala" -> """package orders
class Order(val lines: List[String])
""",
      "Invoice.scala" -> """package billing
class Invoice(val order: orders.Order):
  def total: Int = order.lines.size
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath, "design")
    assert(!result.hasErrors, result.errorMessages)
  }

  test("annotation checks still apply together with design checks") {
    val sources = List(
      "Order.scala" -> """package orders
class Order(val lines: List[String])
""",
      "Invoice.scala" -> """package billing
class Invoice(val order: orders.Order)
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath, "design")
    assert(result.errorMessages.contains("Package billing cannot depend on orders"), result.errorMessages)
  }

  test("annotations cannot legalize a cycle that design rejects") {
    val sources = List(
      "BillingLayer.scala" -> """package billing
@layers.dependsOnPackages("orders")
object layer
""",
      "OrdersLayer.scala" -> """package orders
@layers.dependsOnPackages("billing")
object layer
"""
    ) ++ cyclic
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath, "design")
    assert(result.errorMessages.contains("Cyclic dependency between packages: billing, orders"), result.errorMessages)
  }

  test("nested packages and std lib references do not create cycles") {
    val sources = List(
      "AuthLayer.scala" -> """package app
package auth
object layer
""",
      "WebLayer.scala" -> """package app
package web
@layers.dependsOnPackages("app.auth")
object layer
""",
      "A.scala" -> """package app
package auth
class AuthService(val xs: List[String], val m: Map[String, Int])
""",
      "B.scala" -> """package app
package web
class Controller(val auth: app.auth.AuthService)
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath, "design")
    assert(!result.hasErrors, result.errorMessages)
  }

  test("incremental: cycle introduced by editing one file is found using cached facts of the other") {
    val rounds = List(
      List(
        "billing/layer.scala" -> """package billing
@layers.dependsOnPackages("orders")
object layer
""",
        "orders/layer.scala" -> """package orders
object layer
""",
        "billing/Invoice.scala" -> """package billing
class Invoice(val order: orders.Order):
  def total: Int = order.lines.size
""",
        "orders/Order.scala" -> """package orders
class Order(val lines: List[String]):
  def label: String = "order"
"""
      ),
      List(
        // Body-only change: Invoice is not recompiled, so only cached facts know billing -> orders.
        "orders/Order.scala" -> """package orders
class Order(val lines: List[String]):
  def label: String = "order " + new billing.Invoice(this).total
"""
      ),
      List(
        // Fixed again.
        "orders/Order.scala" -> """package orders
class Order(val lines: List[String]):
  def label: String = "order"
"""
      )
    )
    val results = ZincIncrementalRunner.runCompilationRounds(
      rounds = rounds,
      pluginJarPath = pluginJarPath,
      layersJarPath = layersJarPath,
      extraScalacOptions = Seq("-P:layers:design")
    )
    assertEquals(results.map(_.exitCode), List(0, 1, 0), results.map(_.output).mkString("\n---\n"))
    assert(results(1).output.contains("Cyclic dependency between packages: billing, orders"), results(1).output)
  }

  // domain -> util with util -> lib1, lib2: domain is more stable than util.
  private val unstableDep = List(
    "Domain.scala" -> """package domain
class Money(val u: util.Fmt)
""",
    "Util.scala" -> """package util
class Fmt(val a: lib1.A, val b: lib2.B)
""",
    "Lib1.scala" -> """package lib1
class A
""",
    "Lib2.scala" -> """package lib2
class B
""",
    "App.scala" -> """package app
class Main(val m: domain.Money)
""",
    "DomainLayer.scala" -> """package domain
@layers.dependsOnPackages("util")
object layer
""",
    "UtilLayer.scala" -> """package util
@layers.dependsOnPackages("lib1", "lib2")
object layer
""",
    "AppLayer.scala" -> """package app
@layers.dependsOnPackages("domain")
object layer
""",
    "Lib1Layer.scala" -> """package lib1
object layer
""",
    "Lib2Layer.scala" -> """package lib2
object layer
"""
  )

  test("stable dependencies violation fails compilation") {
    val result = CompilerPluginTestHelper.compile(unstableDep, pluginJarPath, "design")
    assert(result.hasErrors)
    assert(result.errorMessages.contains("Stable Dependencies violation: domain -> util"), result.errorMessages)
  }

  test("baseline accepts legacy violation; init, ratchet and lock-in") {
    val dir = java.nio.file.Files.createTempDirectory("layers-baseline-")
    val file = dir.resolve("layers-baseline.txt")
    val opt = s"baseline=$file"
    def compile(extra: String*) =
      CompilerPluginTestHelper.compile(unstableDep, pluginJarPath, List("design", opt) ++ extra)

    // creating the baseline records current violations
    assert(!compile("updateBaseline").hasErrors)
    assert(java.nio.file.Files.readString(file).contains("sdp domain -> util"))
    // now the legacy violation passes without the flag
    assert(!compile().hasErrors)

    // a violation that is not in the baseline fails
    java.nio.file.Files.writeString(file, "# empty\n")
    val r = compile()
    assert(r.errorMessages.contains("Stable Dependencies violation: domain -> util"), r.errorMessages)
    // updating never adds entries
    compile("updateBaseline")
    assert(!java.nio.file.Files.readString(file).contains("sdp domain -> util"))

    // an entry for a violation that no longer exists fails until locked in
    java.nio.file.Files.writeString(file, "sdp gone -> away\nsdp domain -> util\n")
    val stale = compile()
    assert(stale.errorMessages.contains("out of date"), stale.errorMessages)
    assert(!compile("updateBaseline").hasErrors)
    assert(!java.nio.file.Files.readString(file).contains("gone"))
    assert(!compile().hasErrors)
  }

  test("cycles are reported before stability") {
    val sources = List(
      "A.scala" -> "package a\nclass A(val x: b.B)\n",
      "B.scala" -> "package b\nclass B(val y: a.A)\n"
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath, "design")
    assert(result.errorMessages.contains("Cyclic dependency"), result.errorMessages)
    assert(!result.errorMessages.contains("Stable Dependencies"), result.errorMessages)
  }

  private def design(source: String, opts: String*) =
    CompilerPluginTestHelper.compile(
      List("Layer.scala" -> "package p\nobject layer\n", "Src.scala" -> source),
      pluginJarPath,
      "design" :: opts.toList
    )

  test("simple code has no member findings") {
    val r = design("package p\nclass Calc:\n  def add(a: Int, b: Int): Int = a + b\n")
    assert(!r.hasErrors, r.errorMessages)
  }

  test("cyclomatic complexity over the limit fails with a fix") {
    val r = design(
      """package p
class Calc:
  def f(x: Int): Int =
    if x > 1 then 1
    else if x > 2 then 2
    else if x > 3 then 3
    else if x > 4 then 4
    else 5
""",
      "maxCyclomatic=3"
    )
    assert(r.errorMessages.contains("Method `p.Calc.f` has too many independent paths"), r.errorMessages)
    assert(r.errorMessages.contains("cyclomatic complexity): 5 (limit 3)"), r.errorMessages)
  }

  test("boolean operators and match cases count") {
    val r = design(
      """package p
class Calc:
  def f(x: Int, y: Int): String =
    if x > 1 && y > 1 || x < 0 then
      x match
        case 1 => "a"
        case 2 => "b"
        case _ => "c"
    else "d"
""",
      "maxCyclomatic=5"
    )
    // 1 + if + && + || + (3 cases - 1) = 6
    assert(r.errorMessages.contains("(cyclomatic complexity): 6 (limit 5)"), r.errorMessages)
  }

  test("nesting and parameter count are reported") {
    val r = design(
      """package p
class Calc:
  def f(a: Int, b: Int, c: Int): Int =
    if a > 0 then
      if b > 0 then
        if c > 0 then 1 else 2
      else 3
    else 4
""",
      "maxNesting=2",
      "maxParams=2"
    )
    assert(r.errorMessages.contains("is nested too deeply"), r.errorMessages)
    assert(r.errorMessages.contains("takes too many parameters"), r.errorMessages)
  }

  test("implicit parameters are not counted") {
    val r = design("package p\nclass Calc:\n  def f(a: Int)(using b: String, c: Int): Int = a\n", "maxParams=1")
    assert(!r.hasErrors, r.errorMessages)
  }

  private val animals =
    """package p
case class Cat(n: String)
case class Dog(n: String)
case class Cow(n: String)
"""

  test("class mixing methods that work on different domain types is reported") {
    val r = design(
      animals + """
class Zoo:
  def addCat(c: Cat): Unit = ()
  def cats: Seq[Cat] = Nil
  def addDog(d: Dog): Unit = ()
  def dogs: Seq[Dog] = Nil
  def addCow(c: Cow): Unit = ()
  def cows: Seq[Cow] = Nil
""",
      "maxResponsibilities=2"
    )
    assert(r.errorMessages.contains("Class `p.Zoo` has 3 responsibilities"), r.errorMessages)
    assert(r.errorMessages.contains("[Cat] addCat, cats") && r.errorMessages.contains("[Dog]"), r.errorMessages)
  }

  test("objects are judged like classes, so moving methods into an object does not hide them") {
    val r = design(
      animals + """
object Zoo:
  def addCat(c: Cat): Unit = ()
  def cats: Seq[Cat] = Nil
  def addDog(d: Dog): Unit = ()
  def dogs: Seq[Dog] = Nil
  def addCow(c: Cow): Unit = ()
  def cows: Seq[Cow] = Nil
""",
      "maxResponsibilities=2"
    )
    assert(r.errorMessages.contains("`p.Zoo` has 3 responsibilities"), r.errorMessages)
  }

  test("methods that share a field, a type or call each other are cohesive") {
    val r = design(
      """package p
class Counter:
  private var n = 0
  def inc(): Unit = n += 1
  def twice(): Unit = { inc(); inc() }
  def get: Int = n
  def reset(): Unit = n = 0
""",
      "maxResponsibilities=1"
    )
    assert(!r.hasErrors, r.errorMessages)
  }

  test("a shared concrete helper does not hide unrelated responsibilities") {
    val r = design(
      animals + """
class Zoo:
  private def tx[A](f: => A): A = f
  def addCat(c: Cat): Unit = tx(())
  def cats: Seq[Cat] = tx(Nil)
  def addDog(d: Dog): Unit = tx(())
  def dogs: Seq[Dog] = tx(Nil)
  def addCow(c: Cow): Unit = tx(())
  def cows: Seq[Cow] = tx(Nil)
  def addHen(c: Cat): Unit = tx(())
  def hens: Seq[Cow] = tx(Nil)
""",
      "maxResponsibilities=1"
    )
    assert(r.errorMessages.contains("Class `p.Zoo` has 3 responsibilities"), r.errorMessages)
  }

  test("default argument getters and export forwarders are not methods of the class") {
    val r = design(
      animals + """
object Parts:
  def a(c: Cat): Int = 1
  def b(d: Dog): Int = 2
  def c(c: Cow): Int = 3
  def d(c: Cat, d: Dog): Int = 4
class Zoo:
  def withDefaults(x: Int = 1, y: Int = 2, z: Int = 3): Int = x + y + z
  def withDefaults2(x: Int = 1, y: Int = 2): Int = x + y
  def cats(c: Cat): Int = 1
  def cats2(c: Cat): Int = 2
""",
      "maxResponsibilities=2"
    )
    assert(!r.hasErrors, r.errorMessages)
  }

  test("a class that is mostly exports is a facade") {
    val r = design(
      """package p
object Parts:
  def a: Int = 1
  def b: Int = 2
  def c: Int = 3
  def d: Int = 4
class Facade:
  export Parts.*
  def own: Int = 0
  def mine: Int = 0
""",
      "maxExports=3"
    )
    assert(r.errorMessages.contains("Class `p.Facade` is mostly a facade"), r.errorMessages)
  }

  test("splitting a method into single-use private helpers does not hide its complexity") {
    val branch = (1 to 4).map(i => s"    if x == $i then $i else").mkString("\n")
    val r = design(
      s"""package p
class C:
  def run(x: Int): Int = one(x) + two(x)
  private def one(x: Int): Int =
$branch
    0
  private def two(x: Int): Int =
$branch
    0
""",
      "maxCyclomatic=6", "maxClusterCyclomatic=1"
    )
    assert(r.errorMessages.contains("Method `p.C.run`") && r.errorMessages.contains("single-use helper"), r.errorMessages)
    assert(!r.errorMessages.contains("Method `p.C.one`"), r.errorMessages)
  }

  test("making a single-use helper public does not hide its size") {
    val body = (1 to 30).map(i => s"    val v$i = x + $i + x * $i + x - $i + x * 2").mkString("\n")
    val r = design(
      s"""package p
object C:
  def run(x: Int): Int = part(x)
  def part(x: Int): Int =
$body
    v1
""",
      "maxMethodLines=10", "maxFlatLines=1"
    )
    // too big to be hidden: it is reported itself, wherever it lives and whatever its visibility
    assert(r.errorMessages.contains("Method `p.C$.part` is too long"), r.errorMessages)
  }

  test("making single-use helpers public does not hide that their sum is too long") {
    val body = (1 to 8).map(i => s"    val v$i = x + $i + x * $i + x - $i + x * 2").mkString("\n")
    val r = design(
      s"""package p
object C:
  def run(x: Int): Int = part(x) + other(x)
  def part(x: Int): Int =
$body
    v1
  def other(x: Int): Int =
$body
    v2
""",
      "maxMethodLines=10", "maxFlatLines=1", "maxClusterLines=1"
    )
    assert(r.errorMessages.contains("Method `p.C$.run`") && r.errorMessages.contains("single-use helper"), r.errorMessages)
  }

  test("method size ignores formatting: spreading the same code over more lines gives the same size") {
    def value(body: String): Int =
      val r = design(s"package p\nobject C:\n  def a(x: Int): Int =\n$body\n    x\n", "maxMethodLines=1", "maxFlatLines=1")
      "(?s)too long.*?: (\\d+) \\(limit".r.findFirstMatchIn(r.errorMessages).map(_.group(1).toInt).getOrElse(-1)
    val tight = (1 to 30).map(i => s"    val v$i = x + $i").mkString("\n")
    val spread = (1 to 30).map(i => s"    val v$i =\n      x +\n        $i").mkString("\n")
    assertEquals(value(spread), value(tight))
    assert(value(tight) > 0)
    // comments are not code
    val comments = (1 to 80).map(i => s"    // note $i about nothing").mkString("\n")
    assertEquals(value(comments + "\n" + tight), value(tight))
  }

  test("a val holding a table of lambdas is measured like a method body") {
    val rows = (1 to 8).map(i => s"    (x: Int) => if x > $i then $i else if x < -$i then -$i else 0").mkString(",\n")
    val r = design(
      s"""package p
object T:
  val rules: List[Int => Int] = List(
$rows
  )
""",
      "maxCyclomatic=10"
    )
    assert(r.errorMessages.contains("`p.T$.rules`") && r.errorMessages.contains("independent paths"), r.errorMessages)
  }

  test("top-level definitions of a file are one object and are checked for cohesion too") {
    val r = design(
      """package p
case class A(n: Int)
case class B(n: Int)
case class C(n: Int)
def fa(a: A): Int = a.n
def ga(a: A): Int = a.n + 1
def fb(b: B): Int = b.n
def gb(b: B): Int = b.n + 1
def fc(c: C): Int = c.n
def gc(c: C): Int = c.n + 1
""",
      "maxResponsibilities=2"
    )
    assert(r.errorMessages.contains("responsibilities (limit 2)"), r.errorMessages)
  }

  test("code pasted in by an inline method is not counted at the call site") {
    val branch = (1 to 12).map(i => s"    if x == $i then $i else").mkString("\n")
    val r = design(
      s"""package p
object Rules:
  inline def classify(inline x: Int): Int =
$branch
    0
object Use:
  val a: Int = Rules.classify(3)
  def b(): Int = Rules.classify(4)
""",
      "maxCyclomatic=6"
    )
    assert(!r.errorMessages.contains("Use$"), r.errorMessages)
  }

  test("long text and flat layout are not logic: prose weighs little, flat methods may run longer") {
    val prose = (1 to 40).map(i => s"""    "paragraph $i: ${"lorem ipsum dolor sit amet " * 4}" +""").mkString("\n")
    val r = design(s"package p\nobject T:\n  def text: String =\n$prose\n    \"\"\n", "maxMethodLines=20")
    assert(!r.hasErrors, r.errorMessages)
    val flat = (1 to 60).map(i => s"    val v$i = List(x, $i, x + $i, x * $i)").mkString("\n")
    val flatR = design(s"package p\nobject T:\n  def f(x: Int): Int =\n$flat\n    x\n", "maxMethodLines=20", "maxFlatLines=400")
    assert(!flatR.hasErrors, flatR.errorMessages)
    val logic = (1 to 12).map(i => s"    val v$i = if x > $i then List(x, $i, x + $i) else List(x * $i, x - $i, $i)").mkString("\n")
    val logicR = design(s"package p\nobject T:\n  def f(x: Int): Int =\n$logic\n    x\n", "maxMethodLines=20", "maxFlatLines=400")
    assert(logicR.errorMessages.contains("is too long"), logicR.errorMessages)
  }

  test("a helper used from two methods is reuse, not fragmentation") {
    val branch = (1 to 4).map(i => s"    if x == $i then $i else").mkString("\n")
    val r = design(
      s"""package p
class C:
  def a(x: Int): Int = help(x)
  def b(x: Int): Int = help(x) + 1
  private def help(x: Int): Int =
$branch
    0
""",
      "maxCyclomatic=6"
    )
    assert(!r.hasErrors, r.errorMessages)
  }

  test("a case class built once and read by one method is a parameter object in disguise") {
    val r = design(
      """package p
case class Args(a: Int, b: Int, c: Int, d: Int)
class Svc:
  def run(): Int = work(Args(1, 2, 3, 4))
  private def work(args: Args): Int = args.a + args.b + args.c + args.d
""",
      ""
    )
    assert(r.errorMessages.contains("`p.Args` is a single-use parameter object"), r.errorMessages)
  }

  test("a case class used in several places or with behaviour is a real type") {
    val reused = design(
      """package p
case class Args(a: Int, b: Int, c: Int, d: Int)
class Svc:
  def run(): Int = work(Args(1, 2, 3, 4)) + other(Args(5, 6, 7, 8))
  private def work(args: Args): Int = args.a + args.b + args.c + args.d
  private def other(args: Args): Int = args.a
""",
      ""
    )
    assert(!reused.hasErrors, reused.errorMessages)
    val behaving = design(
      """package p
case class Args(a: Int, b: Int, c: Int, d: Int):
  def total: Int = a + b + c + d
class Svc:
  def run(): Int = work(Args(1, 2, 3, 4))
  private def work(args: Args): Int = args.total
""",
      ""
    )
    assert(!behaving.hasErrors, behaving.errorMessages)
  }

  test("a wrapper that reorders parameters or adds one constant is still a pass-through") {
    val r = design(
      """package p
class Repo:
  def find(id: Long, name: String, mode: Int): String = name + id + mode
class Service(repo: Repo):
  def lookup(name: String, id: Long): String = repo.find(id, name, 0)
""",
      ""
    )
    assert(r.errorMessages.contains("only forwards"), r.errorMessages)
  }

  private def sh(dir: java.nio.file.Path, args: String*): Unit =
    val p = new ProcessBuilder((List("git", "-C", dir.toString, "-c", "user.name=t", "-c", "user.email=t@t") ++ args)*)
      .redirectErrorStream(true).start()
    p.getInputStream.readAllBytes()
    assertEquals(p.waitFor(), 0)

  test("since=<rev> enforces limits only on code changed since that revision") {
    val dir = java.nio.file.Files.createTempDirectory("layers-ratchet-")
    def branchy(tag: String, n: Int) =
      s"  def $tag(x: Int): Int =\n" + (1 to n).map(i => s"    if x == $i then $i else").mkString("\n") + "\n    0\n"
    def src(oldBody: String) = "package p\nclass Old:\n" + oldBody + "class Other:\n" + branchy("untouched", 6)
    def compile(files: List[(String, String)]) =
      CompilerPluginTestHelper.compile(files, pluginJarPath, List("design", "maxCyclomatic=3", "since=HEAD"), sourceDir = Some(dir))
    val layer = "Layer.scala" -> "package p\nobject layer\n"
    compile(List(layer, "Src.scala" -> src(branchy("run", 6)))) // write the files
    sh(dir, "init", "-q")
    sh(dir, "add", ".")
    sh(dir, "commit", "-q", "-m", "legacy")

    val unchanged = compile(List(layer, "Src.scala" -> src(branchy("run", 6))))
    assert(!unchanged.hasErrors, unchanged.errorMessages)
    assert(unchanged.warningMessages.contains("existing findings"), unchanged.warningMessages)

    val edited = compile(List(layer, "Src.scala" -> src(branchy("run", 7))))
    assert(edited.hasErrors && edited.errorMessages.contains("p.Old.run"), edited.errorMessages)
    assert(!edited.errorMessages.contains("untouched"), edited.errorMessages)

    val added = compile(List(layer, "Src.scala" -> src(branchy("run", 6)), "New.scala" -> ("package p\nclass Fresh:\n" + branchy("go", 6))))
    assert(added.hasErrors && added.errorMessages.contains("Method `p.Fresh.go`") && !added.errorMessages.contains("Method `p.Old.run`"), added.errorMessages)
  }

  test("member baseline: accepts worst value, rejects regression") {
    val dir = java.nio.file.Files.createTempDirectory("layers-baseline-")
    val file = dir.resolve("b.txt")
    def src(branches: Int) =
      "package p\nclass C:\n  def f(x: Int): Int =\n" +
        (1 to branches).map(i => s"    if x == $i then $i else").mkString("\n") + "\n    0\n"
    def compile(branches: Int, extra: String*) = design(src(branches), (List("maxCyclomatic=3", s"baseline=$file") ++ extra)*)

    assert(!compile(5, "updateBaseline").hasErrors)
    assert(java.nio.file.Files.readString(file).contains("cyclomatic p.C.f = 6"))
    assert(!compile(5).hasErrors)
    assert(compile(6).hasErrors) // worse than baseline
    val improved = compile(4)
    assert(improved.errorMessages.contains("improved from 6 to 5"), improved.errorMessages)
    assert(!compile(4, "updateBaseline").hasErrors)
    assert(java.nio.file.Files.readString(file).contains("cyclomatic p.C.f = 5"))
  }

  test("pass-through wrapper is reported, transforming wrapper is not") {
    val r = design(
      """package p
class Repo:
  def find(id: String): String = id
class Service(repo: Repo):
  def find(id: String): String = repo.find(id)
  def findTrimmed(id: String): String = repo.find(id.trim)
"""
    )
    assert(r.errorMessages.contains("Method `p.Service.find` only forwards its parameters to `p.Repo.find`"), r.errorMessages)
    assert(!r.errorMessages.contains("findTrimmed"), r.errorMessages)
  }

  test("delegating an inherited method (adapter) is not a pass-through") {
    val r = design(
      """package p
trait Port:
  def get(id: String): String
class Repo:
  def load(id: String): String = id
class Adapter(repo: Repo) extends Port:
  def get(id: String): String = repo.load(id)
class Other(repo: Repo) extends Port:
  def get(id: String): String = repo.load(id) + "!"
"""
    )
    assert(!r.errorMessages.contains("only forwards"), r.errorMessages)
  }

  private val cloneA = """package p
class A:
  def total(xs: List[Int]): Int =
    var acc = 0
    for x <- xs do
      if x > 10 then acc += x * 2 else acc -= 1
    acc
"""

  test("copy-pasted method with renamed locals and other literals is a clone") {
    val r = design(
      cloneA + """
class B:
  def sum(items: List[Int]): Int =
    var result = 0
    for item <- items do
      if item > 99 then result += item * 3 else result -= 7
    result
""",
      "minCloneSize=20"
    )
    assert(r.errorMessages.contains("Duplicated code: 2 methods have the same logic"), r.errorMessages)
    assert(r.errorMessages.contains("p.A.total") && r.errorMessages.contains("p.B.sum"), r.errorMessages)
  }

  test("methods with different logic are not clones") {
    val r = design(
      cloneA + """
class B:
  def sum(items: List[Int]): Int =
    var result = 0
    for item <- items do
      if item > 99 then result += item.max(3) else result -= 7
    result
""",
      "minCloneSize=20"
    )
    assert(!r.errorMessages.contains("Duplicated code"), r.errorMessages)
  }

  test("trait with a single same-package implementation is reported") {
    val r = design(
      """package p
trait Store:
  def get: Int
class MemStore extends Store:
  def get: Int = 1
"""
    )
    assert(r.errorMessages.contains("Trait `p.Store` has a single implementation, `p.MemStore`"), r.errorMessages)
  }

  test("trait implemented in another package is a legitimate boundary") {
    val r = CompilerPluginTestHelper.compile(
      List(
        "PLayer.scala" -> "package p\nobject layer\n",
        "QLayer.scala" -> "package q\n@layers.dependsOnPackages(\"p\")\nobject layer\n",
        "Store.scala" -> "package p\ntrait Store:\n  def get: Int\n",
        "Mem.scala" -> "package q\nclass MemStore extends p.Store:\n  def get: Int = 1\n"
      ),
      pluginJarPath,
      "design"
    )
    assert(!r.hasErrors, r.errorMessages)
  }

  test("trait with two implementations is fine") {
    val r = design(
      """package p
trait Store:
  def get: Int
class A extends Store:
  def get: Int = 1
class B extends Store:
  def get: Int = 2
"""
    )
    assert(!r.hasErrors, r.errorMessages)
  }

  test("lookup tables that differ only in literals are not clones") {
    val r = design(
      """package p
enum Metodo:
  case Goccia
  case Aspersione
  case Scorrimento
  case Sommersione
  case Manuale
  case Altro

  def nome: String = this match
    case Goccia => "A goccia"
    case Aspersione => "Aspersione"
    case Scorrimento => "Scorrimento"
    case Sommersione => "Sommersione"
    case Manuale => "Manuale"
    case Altro => "Altro"

  def id: String = this match
    case Goccia => "goccia"
    case Aspersione => "aspersione"
    case Scorrimento => "scorrimento"
    case Sommersione => "sommersione"
    case Manuale => "manuale"
    case Altro => "altro"
""",
      "minCloneSize=20"
    )
    assert(!r.errorMessages.contains("Duplicated code"), r.errorMessages)
  }

  test("cycle error suggests extracting the shared class") {
    val r = CompilerPluginTestHelper.compile(
      List(
        "Api.scala" -> "package app\nclass Api(val a: auth.Auth)\n",
        "Auth.scala" -> "package auth\nclass Auth(val u: app.User)\n",
        "User.scala" -> "package app\nclass User(val name: String)\n"
      ),
      pluginJarPath,
      "design"
    )
    assert(r.errorMessages.contains("Suggested restructuring"), r.errorMessages)
    assert(r.errorMessages.contains("app.User"), r.errorMessages)
    assert(r.errorMessages.contains("the cycle is gone"), r.errorMessages)
  }
