package layers.plugin

import munit.FunSuite

import ArchTestSupport.design

class LayerMixTests extends FunSuite:

  private val layers = List(
    "Db.scala" -> "package shop.database\nobject Db:\n  def load(id: Int): String = id.toString\n  def save(s: String): Unit = ()\n",
    "Html.scala" -> "package shop.rendering\nobject Html:\n  def page(s: String): String = s\n",
    "Pay.scala" -> "package shop.stripe\nobject Pay:\n  def charge(n: Int): Unit = ()\n",
    "Order.scala" -> "package shop.model\ncase class Order(raw: String):\n  def total: Int = raw.length\n  def tax: Int = raw.length / 5\n"
  )

  private def handler(body: String) = "Handler.scala" -> s"package shop.app\nobject Handler:\n$body\n"

  private val bloated = handler("""  def pay(r: com.sun.net.httpserver.HttpExchange): String =
    val o = shop.model.Order(shop.database.Db.load(r.getRequestMethod.length))
    if o.total > 100 then shop.stripe.Pay.charge(o.total + o.tax) else shop.stripe.Pay.charge(100)
    if o.total > 0 then shop.database.Db.save("paid")
    if o.total > 5 && o.total < 50 then shop.rendering.Html.page(o.total.toString) else "none"""")

  test("a method that parses the request, writes the database, calls a service and renders is reported by layer") {
    val m = design(layers :+ bloated, "domain=shop.model").errorMessages
    assert(m.contains("Method `shop.app.Handler.pay` mixes 5 layers (limit 4)"), m)
    assert(m.contains("web/routing: httpserver.HttpExchange.getRequestMethod"), m)
    assert(m.contains("persistence: database.Db.load, database.Db.save"), m)
    assert(m.contains("external service: stripe.Pay.charge"), m)
    assert(m.contains("rendering/view: rendering.Html.page"), m)
    assert(m.contains("domain: model.Order.total"), m)
    assert(m.contains("Separate the steps"), m)
  }

  test("a method that touches two layers is fine") {
    val ok = handler("""  def load(id: Int): String =
    if id > 1 then shop.rendering.Html.page(shop.database.Db.load(id)) else if id > 2 then "a" else if id > 3 then "b" else "c"""")
    assert(!design(layers :+ ok, "domain=shop.model").errorMessages.contains("mixes"))
  }

  test("a straight-line orchestrator is not judged: it has no decisions of its own mixed in with the side effects") {
    val straight = handler("""  def pay(r: com.sun.net.httpserver.HttpExchange): String =
    val o = shop.model.Order(shop.database.Db.load(r.getRequestMethod.length))
    shop.stripe.Pay.charge(o.total + o.tax)
    shop.rendering.Html.page(o.total.toString)""")
    assert(!design(layers :+ straight, "domain=shop.model").errorMessages.contains("mixes"))
    assert(design(layers :+ straight, "domain=shop.model", "layerMixMinCyclomatic=1").errorMessages.contains("mixes 5 layers"))
  }

  test("cutting the method into single-use helpers keeps the layers together") {
    val cut = handler("""  def pay(r: com.sun.net.httpserver.HttpExchange): String =
    val o = loadOrder(r)
    charge(o)
    show(o)
  private def loadOrder(r: com.sun.net.httpserver.HttpExchange): shop.model.Order = if r.getRequestMethod.length > 0 then shop.model.Order(shop.database.Db.load(r.getRequestMethod.length)) else shop.model.Order("")
  private def charge(o: shop.model.Order): Unit = if o.total > 1 then shop.stripe.Pay.charge(o.total) else if o.total > 0 then shop.stripe.Pay.charge(1)
  private def show(o: shop.model.Order): String = if o.total > 3 then shop.rendering.Html.page(o.total.toString) else "none"""")
    val m = design(layers :+ cut, "domain=shop.model").errorMessages
    assert(m.contains("Method `shop.app.Handler.pay` mixes"), m)
    assert(m.contains("counting 3 single-use helpers"), m)
  }

  test("helpers shared by several methods are real separation: each is judged on its own") {
    val shared = handler("""  def pay(r: com.sun.net.httpserver.HttpExchange): String = if r.getRequestMethod.length > 1 then show(load(r)) else if r.getRequestMethod.length > 2 then "a" else if r.getRequestMethod.length > 3 then "b" else "c"
  def preview(r: com.sun.net.httpserver.HttpExchange): String = show(load(r))
  private def load(r: com.sun.net.httpserver.HttpExchange): shop.model.Order = shop.model.Order(shop.database.Db.load(r.getRequestMethod.length))
  private def show(o: shop.model.Order): String = shop.rendering.Html.page(o.total.toString)""")
    assert(!design(layers :+ shared, "domain=shop.model").errorMessages.contains("mixes"))
  }

  test("a single call into the domain is a helper, not the domain logic of the handler") {
    val oneCall = handler("""  def pay(r: com.sun.net.httpserver.HttpExchange): String =
    val o = shop.model.Order(shop.database.Db.load(r.getRequestMethod.length))
    if o.total > 100 then shop.database.Db.save("a") else if o.total > 5 then shop.database.Db.save("b") else if o.total > 7 then "c" else "d"""")
    // web + persistence + domain (one call only) is two layers
    assert(!design(layers :+ oneCall, "domain=shop.model").errorMessages.contains("mixes"))
  }

  test("the limit is an option") {
    assert(!design(layers :+ bloated, "domain=shop.model", "maxLayersPerMethod=6").errorMessages.contains("mixes"))
    assert(design(layers :+ bloated, "domain=shop.model", "maxLayersPerMethod=5").errorMessages.contains("mixes 5 layers"))
  }

  test("package kinds can be overridden") {
    val sources = List(
      "Dao.scala" -> "package shop.things\nobject Dao:\n  def load(id: Int): String = id.toString\n",
      "Order.scala" -> "package shop.model\ncase class Order(raw: String):\n  def shout: String = raw\n  def whisper: String = raw.toLowerCase\n",
      handler("""  def a(id: Int): String =
    val o = shop.model.Order(shop.things.Dao.load(id))
    if id > 1 && id < 5 || id == 9 then shop.rendering.Html.page(o.shout + o.whisper) else if id > 100 then "a" else "b""""),
      "Html.scala" -> "package shop.rendering\nobject Html:\n  def page(s: String): String = s\n"
    )
    assert(!design(sources, "domain=shop.model").errorMessages.contains("mixes"))
    assert(design(sources, "domain=shop.model", "layerKinds=shop.things:persistence", "maxLayersPerMethod=3").errorMessages.contains("mixes 3 layers"))
  }

  test("changing layerKinds after files were extracted asks for a clean compile") {
    val dir = java.nio.file.Files.createTempDirectory("arch-out")
    val src = java.nio.file.Files.createTempDirectory("arch-src")
    val first = CompilerPluginTestHelper.compile(layers :+ bloated, ArchTestSupport.pluginJar, List("design", "domain=shop.model"), outputDir = Some(dir), sourceDir = Some(src))
    assert(first.errorMessages.contains("mixes"))
    val second = CompilerPluginTestHelper.compile(layers.take(1), ArchTestSupport.pluginJar, List("design", "domain=shop.model", "layerKinds=shop.x:web"), outputDir = Some(dir), sourceDir = Some(src))
    assert(second.errorMessages.contains("extracted with other identityTypes/layerKinds options"), second.errorMessages)
  }

  test("the default limit is four layers: a handler over web, persistence and rendering is a normal transaction script") {
    val prg = handler("""  def save(r: com.sun.net.httpserver.HttpExchange): String =
    val id = r.getRequestMethod.length
    if id > 100 then shop.database.Db.save("a") else if id > 5 then shop.database.Db.save("b") else if id > 7 then "c" else "d"
    shop.rendering.Html.page(shop.database.Db.load(id))""")
    assert(!design(layers :+ prg, "domain=shop.model").errorMessages.contains("mixes"))
    assert(design(layers :+ prg, "domain=shop.model", "maxLayersPerMethod=3").errorMessages.contains("mixes 3 layers"))
  }

  test("an orchestrator inside an integration package is the integration, not a caller of an external service") {
    val parts = List(
      "Client.scala" -> "package shop.stripe.client\nobject Client:\n  def call(n: Int): Int = n\n",
      "Telegram.scala" -> "package shop.telegram\nobject Telegram:\n  def send(s: String): Unit = ()\n"
    )
    def sync(extra: String) = "Sync.scala" -> s"""package shop.stripe
object Sync:
  def run(n: Int): String =
    val o = shop.model.Order(shop.database.Db.load(shop.stripe.client.Client.call(n)))
    if o.total > 100 then shop.database.Db.save("a") else if o.total > 5 then shop.database.Db.save("b") else if o.total > 7 then "c" else "d"
    $extra
    shop.rendering.Html.page(o.total.toString + o.tax)
"""
    // persistence, rendering and domain: its own integration package and the client under it are not "external"
    assert(!design(layers ++ parts :+ sync("()"), "domain=shop.model", "maxLayersPerMethod=4").errorMessages.contains("mixes"))
    // calling another integration is
    val m = design(layers ++ parts :+ sync("shop.telegram.Telegram.send(\"x\")"), "domain=shop.model", "maxLayersPerMethod=4").errorMessages
    assert(m.contains("Method `shop.stripe.Sync.run` mixes 4 layers"), m)
  }

