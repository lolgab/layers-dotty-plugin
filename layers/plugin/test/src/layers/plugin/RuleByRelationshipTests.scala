package layers.plugin

import munit.FunSuite

import ArchTestSupport.design

class RuleByRelationshipTests extends FunSuite:

  private val lot = "Lot.scala" -> "package shop.model\ncase class Lot(id: Int, liters: Int)\n"

  private def tank(body: String) = "Tank.scala" -> s"package shop.model\ncase class Tank(lots: List[Lot], max: Int):\n$body\n"

  private val counting = tank("  def canAdd: Boolean = lots.size < max\n  def has(id: Int): Boolean = lots.exists(_.id == id)")

  test("a collection held only to count or ask for membership is reported when the check is on") {
    val m = design(List(lot, counting), "domain=shop.model", "ruleByRelationship").errorMessages
    assert(m.contains("`shop.model.Tank` holds `lots`, a collection of `Lot`, only to exists/size it"), m)
    assert(m.contains("Keep only what the rule needs"), m)
    assert(m.contains("Enforce the rule in a query"), m)
  }

  test("it is off by default") {
    assert(!design(List(lot, counting), "domain=shop.model").errorMessages.contains("only to"))
  }

  test("a collection that is also read, mapped or passed on is real data") {
    val summing = tank("  def canAdd: Boolean = lots.size < max\n  def total: Int = lots.map(_.liters).sum")
    assert(!design(List(lot, summing), "domain=shop.model", "ruleByRelationship").errorMessages.contains("only to"))
    val exposed = List(lot, counting, "Use.scala" -> "package shop.web\nobject Use:\n  def all(t: shop.model.Tank): List[shop.model.Lot] = t.lots\n")
    assert(!design(exposed, "domain=shop.model", "ruleByRelationship").errorMessages.contains("only to"))
  }

  test("a collection no method of the class looks at is just data") {
    val data = tank("  def label: String = max.toString")
    assert(!design(List(lot, data), "domain=shop.model", "ruleByRelationship").errorMessages.contains("only to"))
  }
