package layers.plugin

import munit.FunSuite

import ArchTestSupport.design

class ScatteredTransitionTests extends FunSuite:

  private val order = "Order.scala" -> """package shop.model
enum Status:
  case Open, Closed
case class Order(id: Int, status: Status, archived: Boolean = false)
"""

  private def klass(pkg: String, name: String, methods: String*) =
    s"$name.scala" -> (s"package $pkg\nimport shop.model.*\nobject $name:\n" + methods.mkString("\n") + "\n")

  private val three = List(
    order,
    klass("shop.web", "Handlers", "  def close(o: Order): Order = o.copy(status = Status.Closed)", "  def finish(o: Order): Order = o.copy(status = Status.Closed, archived = false)"),
    klass("shop.jobs", "Cleanup", "  def sweep(o: Order): Order = o.copy(status = Status.Closed)")
  )

  test("the same transition in three methods of two classes is reported, with the sites and the fix") {
    val r = design(three, "domain=shop.model")
    val m = r.errorMessages
    assert(m.contains("State transition `status = shop.model.Status.Closed` of `Order` is written out in 3 methods of 2 classes"), m)
    assert(m.contains("Handlers.close") && m.contains("Handlers.finish") && m.contains("Cleanup.sweep"), m)
    assert(m.contains("anemic domain model"), m)
    assert(m.contains("add a method to `Order`"), m)
  }

  test("literal values are transitions too") {
    val r = design(
      List(order, klass("shop.web", "A", "  def a(o: Order) = o.copy(archived = true)", "  def b(o: Order) = o.copy(archived = true)"), klass("shop.jobs", "B", "  def c(o: Order) = o.copy(archived = true)")),
      "domain=shop.model"
    )
    assert(r.errorMessages.contains("State transition `archived = true` of `Order`"), r.errorMessages)
  }

  test("fewer than three methods, a single class, different values or computed values are not reported") {
    val two = design(three.take(2), "domain=shop.model")
    assert(!two.errorMessages.contains("State transition"), two.errorMessages)
    val oneClass = design(List(order, klass("shop.web", "A", "  def a(o: Order) = o.copy(archived = true)", "  def b(o: Order) = o.copy(archived = true)", "  def c(o: Order) = o.copy(archived = true)")), "domain=shop.model")
    assert(!oneClass.errorMessages.contains("State transition"), oneClass.errorMessages)
    val different = design(List(order, klass("shop.web", "A", "  def a(o: Order) = o.copy(archived = true)", "  def b(o: Order) = o.copy(archived = false)"), klass("shop.jobs", "B", "  def c(o: Order) = o.copy(id = o.id + 1)", "  def d(o: Order, n: Int) = o.copy(id = n)")), "domain=shop.model")
    assert(!different.errorMessages.contains("State transition"), different.errorMessages)
  }

  test("a behaviour method on the type is the fix: callers use it and nothing is scattered") {
    val fixed = List(
      "Order.scala" -> """package shop.model
enum Status:
  case Open, Closed
case class Order(id: Int, status: Status, archived: Boolean = false):
  def close: Order = copy(status = Status.Closed)
""",
      klass("shop.web", "Handlers", "  def close(o: Order): Order = o.close", "  def finish(o: Order): Order = o.close"),
      klass("shop.jobs", "Cleanup", "  def sweep(o: Order): Order = o.close")
    )
    assert(!design(fixed, "domain=shop.model").errorMessages.contains("State transition"))
  }

  test("a case class outside the domain is not an anemic model") {
    val r = design(three, "domain=shop.other")
    assert(!r.errorMessages.contains("State transition"), r.errorMessages)
  }

  test("the threshold is an option") {
    assert(!design(three, "domain=shop.model", "minTransitionSites=4").errorMessages.contains("State transition"))
  }

  test("the baseline accepts a legacy transition") {
    val file = java.nio.file.Files.createTempFile("arch-baseline", ".txt")
    java.nio.file.Files.delete(file)
    design(three, "domain=shop.model", s"baseline=$file", "updateBaseline")
    assert(java.nio.file.Files.readString(file).contains("scattered-transition shop.model.Order.status=shop.model.Status.Closed = 3"), java.nio.file.Files.readString(file))
    assert(!design(three, "domain=shop.model", s"baseline=$file").errorMessages.contains("State transition"))
  }
