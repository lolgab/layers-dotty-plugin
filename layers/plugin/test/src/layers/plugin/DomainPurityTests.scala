package layers.plugin

import munit.FunSuite

import ArchTestSupport.design

class DomainPurityTests extends FunSuite:

  private def order(body: String) = List(
    "Order.scala" -> s"""package shop.model
import java.time.LocalDate
$body
"""
  )

  private val clockInDomain = order("""class Order(val placed: LocalDate):
  def overdue: Boolean = placed.isBefore(LocalDate.now())
""")

  test("domain code that reads the clock is reported with the site, the principle and fixes") {
    val r = design(clockInDomain, "domain=shop.model")
    assert(r.hasErrors, "expected a finding")
    val m = r.errorMessages
    assert(m.contains("Domain code `shop.model.Order` is not pure"), m)
    assert(m.contains("reads the clock, `LocalDate.now`"), m)
    assert(m.contains("shop.model.Order.overdue"), m)
    assert(m.contains("Fix with one of"), m)
    assert(m.contains("Pass the value in as a parameter"), m)
    assert(m.contains("pureExempt"), m)
  }

  test("the same code outside the domain is not reported") {
    val r = design(
      List("Order.scala" -> "package shop.web\nimport java.time.LocalDate\nclass Order(val placed: LocalDate):\n  def overdue: Boolean = placed.isBefore(LocalDate.now())\n"),
      "domain=shop.model"
    )
    assert(!r.errorMessages.contains("not pure"), r.errorMessages)
  }

  test("passing the time in as a parameter makes the domain pure") {
    val r = design(order("""class Order(val placed: LocalDate):
  def overdue(today: LocalDate): Boolean = placed.isBefore(today)
"""), "domain=shop.model")
    assert(!r.errorMessages.contains("not pure"), r.errorMessages)
  }

  test("a default argument that reads the clock is still the domain reading the clock") {
    val r = design(order("""class Order(val placed: LocalDate):
  def overdue(today: LocalDate = LocalDate.now()): Boolean = placed.isBefore(today)
"""), "domain=shop.model")
    assert(r.errorMessages.contains("Domain code `shop.model.Order` is not pure"), r.errorMessages)
    assert(r.errorMessages.contains("(default argument)"), r.errorMessages)
  }

  test("randomness, ids, files, the environment, the console, logging and sleeping are all impure") {
    val r = design(order("""object Gen:
  def a = scala.util.Random.nextInt(3)
  def b = java.util.UUID.randomUUID().toString
  def c = java.nio.file.Files.readString(java.nio.file.Path.of("x"))
  def d = System.getenv("HOME")
  def e = println("hi")
  def f = java.util.logging.Logger.getLogger("x").info("m")
  def g = Thread.sleep(10)
  def h = System.currentTimeMillis()
  def i = new java.util.Random().nextInt()
"""), "domain=shop.model")
    val m = r.errorMessages
    for x <- List("draws random values or ids", "`UUID.randomUUID`", "reads or writes files", "reads the environment", "writes to the console", "writes logs", "sleeps or starts threads", "reads the clock, `System.currentTimeMillis`") do
      assert(m.contains(x), s"$x\n$m")
    assert(m.contains("9 uses"), m)
  }

  test("a clock, random source or connection taken as a parameter is the same dependency") {
    val r = design(order("""class Pricing(clock: java.time.Clock, rnd: java.util.Random, conn: java.sql.Connection):
  def price(n: Int): Int = n
"""), "domain=shop.model")
    val m = r.errorMessages
    assert(m.contains("`Clock`") && m.contains("`Random`") && m.contains("uses the database layer, `Connection`"), m)
  }

  test("depending on a database or web package of the project is impure") {
    val r = design(
      List(
        "Db.scala" -> "package shop.database\nobject Db:\n  def find(id: Int): String = id.toString\n",
        "Order.scala" -> "package shop.model\nclass Order:\n  def name(id: Int): String = shop.database.Db.find(id)\n"
      ),
      "domain=shop.model"
    )
    assert(r.errorMessages.contains("uses the database layer, `Db.find`"), r.errorMessages)
  }

  test("moving the clock into a helper outside the domain does not make it pass") {
    val sources = List(
      "Clock.scala" -> "package shop.util\nimport java.time.LocalDate\nobject Today:\n  def get: LocalDate = LocalDate.now()\n",
      "Order.scala" -> "package shop.model\nimport java.time.LocalDate\nclass Order(val placed: LocalDate):\n  def overdue: Boolean = placed.isBefore(shop.util.Today.get)\n"
    )
    val r = design(sources, "domain=shop.model")
    val m = r.errorMessages
    assert(m.contains("Domain code `shop.model.Order` is not pure"), m)
    assert(m.contains("through `shop.util.Today.get`"), m)
    // a deliberate seam is exempted by name, visibly
    val exempt = design(sources, "domain=shop.model", "pureExempt=shop.util.Today")
    assert(!exempt.errorMessages.contains("not pure"), exempt.errorMessages)
  }

  test("an exemption names specific symbols, not the whole domain") {
    val r = design(order("""class Order(val placed: LocalDate):
  def overdue: Boolean = placed.isBefore(LocalDate.now())
  def other: Boolean = placed.isBefore(LocalDate.now())
"""), "domain=shop.model", "pureExempt=shop.model.Order.overdue")
    assert(r.errorMessages.contains("shop.model.Order.other"), r.errorMessages)
    assert(!r.errorMessages.contains("shop.model.Order.overdue"), r.errorMessages)
  }

  test("the baseline accepts legacy findings and fails on new ones") {
    val file = java.nio.file.Files.createTempFile("arch-baseline", ".txt")
    java.nio.file.Files.delete(file)
    val first = design(clockInDomain, "domain=shop.model", s"baseline=$file", "updateBaseline")
    assert(!first.hasErrors || !first.errorMessages.contains("not pure"), first.errorMessages)
    val content = java.nio.file.Files.readString(file)
    assert(content.contains("domain-purity shop.model.Order = 1"), content)
    val again = design(clockInDomain, "domain=shop.model", s"baseline=$file")
    assert(!again.errorMessages.contains("not pure"), again.errorMessages)
    val worse = design(order("""class Order(val placed: LocalDate):
  def overdue: Boolean = placed.isBefore(LocalDate.now())
  def soon: Boolean = placed.isBefore(LocalDate.now().plusDays(2))
  def rnd: Int = scala.util.Random.nextInt()
"""), "domain=shop.model", s"baseline=$file")
    assert(worse.errorMessages.contains("not pure"), worse.errorMessages)
  }

  test("a package named model is found as the domain without an option, a package depending on the database is not") {
    val sources = List(
      "Db.scala" -> "package shop.database\nobject Db:\n  def find(id: Int): String = id.toString\n",
      "Order.scala" -> "package shop.model\nimport java.time.LocalDate\nclass Order:\n  def d = LocalDate.now()\n",
      "Feature.scala" -> "package shop.reports\nimport java.time.LocalDate\nclass Report:\n  def d = LocalDate.now()\n  def n = shop.database.Db.find(1)\n"
    )
    val r = design(sources)
    assert(r.errorMessages.contains("Domain code `shop.model.Order` is not pure"), r.errorMessages)
    assert(!r.errorMessages.contains("shop.reports.Report"), r.errorMessages)
  }

  test("domain facts survive the codec") {
    val f = FileFacts("F.scala", Set("p"), Nil, arch = List(ArchFact("impure", "F.scala", 4, List("p.A.m", "clock", "LocalDate.now", "p", "p.A"))))
    assertEquals(FileFacts.decode(FileFacts.encode(f)).map(_.copy(source = "F.scala")), Some(f))
  }
