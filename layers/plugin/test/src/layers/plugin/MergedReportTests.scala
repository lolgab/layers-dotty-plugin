package layers.plugin

import java.nio.file.Files

import munit.FunSuite

import ArchTestSupport.design

/** The checks of the different plugin parts work together: one report, one place for positions. */
class MergedReportTests extends FunSuite:

  private val sources = List(
    "Db.scala" -> "package shop.database\nobject Db:\n  def load(id: Int): String = id.toString\n  def save(s: String): Unit = ()\n",
    "Html.scala" -> "package shop.rendering\nobject Html:\n  def page(s: String): String = s\n",
    "Pay.scala" -> "package shop.stripe\nobject Pay:\n  def charge(n: Int): Unit = ()\n",
    "Order.scala" -> "package shop.model\ncase class Order(raw: String):\n  def total: Int = raw.length\n  def tax: Int = raw.length / 5\n  def today: java.time.LocalDate = java.time.LocalDate.now()\n",
    "Handler.scala" ->
      """package shop.app
object Handler:
  def pay(r: com.sun.net.httpserver.HttpExchange): String =
    val o = shop.model.Order(shop.database.Db.load(r.getRequestMethod.length))
    if o.total > 100 then shop.stripe.Pay.charge(o.total + o.tax) else shop.stripe.Pay.charge(100)
    if o.total > 0 then shop.database.Db.save("paid")
    if o.total > 5 && o.total < 50 then shop.rendering.Html.page(o.total.toString) else "none"
"""
  )

  test("findings of every part show up for one method, and the report lists values of all of them") {
    val file = Files.createTempDirectory("merged-report").resolve("r.tsv")
    val r = design(sources, "domain=shop.model", "maxCyclomatic=2", "maxLayersPerMethod=4", "report=" + file)
    assert(r.errorMessages.contains("independent paths"), r.errorMessages)
    assert(r.errorMessages.contains("mixes 5 layers"), r.errorMessages)
    assert(r.errorMessages.toLowerCase.contains("clock"), r.errorMessages)
    val lines = Files.readAllLines(file).toArray.toList.map(_.toString)
    def row(kind: String, sym: String) = lines.find(_.startsWith(s"$kind\t$sym\t")).map(_.split('\t').toList)
    val mix = row("layer-mix", "shop.app.Handler.pay").get
    assertEquals(mix.slice(2, 4), List("5", "4"))
    assert(mix(5).contains("web") && mix(5).contains("persistence"), mix.toString)
    assert(lines.exists(_.startsWith("slice-fanout\t")), lines.toString)
    assert(lines.exists(_.startsWith("slice-fanin\t")), lines.toString)
    assert(lines.exists(l => l.startsWith("domain-purity\t") && l.endsWith("\tfinding")), lines.filter(_.contains("purity")).toString)
    assert(lines.exists(_.startsWith("cyclomatic\tshop.app.Handler$.pay\t")), lines.take(10).toString)
  }
