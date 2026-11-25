package layers.plugin

import munit.FunSuite

import ArchTestSupport.design

class GodTypeTests extends FunSuite:

  private val customer = "Customer.scala" -> """package shop.model
case class Customer(
    id: Int, name: String, email: String,
    billingAddress: String, vatId: String, iban: String,
    shippingAddress: String, carrier: String, deliveryNotes: String,
    newsletter: Boolean, segment: String, lastCampaign: String
)
"""

  private def billing(body: String) = "Billing.scala" -> s"package shop.billing\nimport shop.model.Customer\nobject Billing:\n$body\n"
  private def shipping(body: String) = "Shipping.scala" -> s"package shop.shipping\nimport shop.model.Customer\nobject Shipping:\n$body\n"
  private def marketing(body: String) = "Marketing.scala" -> s"package shop.marketing\nimport shop.model.Customer\nobject Marketing:\n$body\n"

  private val split = List(
    customer,
    billing("  def invoice(c: Customer): String = c.billingAddress + c.vatId + c.iban + c.name\n"),
    shipping("  def label(c: Customer): String = c.shippingAddress + c.carrier + c.deliveryNotes + c.name\n"),
    marketing("  def mail(c: Customer): String = c.email + c.newsletter + c.segment + c.lastCampaign\n")
  )

  test("a type with many fields that three packages use for different parts is a god type, with the natural split") {
    val r = design(split, "domain=shop.model")
    val m = r.errorMessages
    assert(m.contains("God type `shop.model.Customer`: 12 fields, referenced from 3 other packages"), m)
    assert(m.contains("shop.billing: billingAddress, iban, name, vatId"), m)
    assert(m.contains("shop.shipping: carrier, deliveryNotes, name, shippingAddress"), m)
    assert(m.contains("shop.marketing: email, lastCampaign, newsletter, segment"), m)
    assert(m.contains("Split by use case or workflow"), m)
    assert(m.contains("Fix with one of"), m)
  }

  test("packages that all use the same fields are one use case: not reported") {
    val same = "c.billingAddress + c.vatId + c.iban + c.name"
    val r = design(
      List(customer, billing(s"  def a(c: Customer): String = $same\n"), shipping(s"  def b(c: Customer): String = $same\n"), marketing(s"  def c(c: Customer): String = $same\n")),
      "domain=shop.model"
    )
    assert(!r.errorMessages.contains("God type"), r.errorMessages)
  }

  test("fewer than three packages, or fewer than twelve fields, is not a god type") {
    val two = design(split.take(3), "domain=shop.model")
    assert(!two.errorMessages.contains("God type"), two.errorMessages)
    assert(!design(split, "domain=shop.model", "godTypeMinFields=13").errorMessages.contains("God type"))
    assert(design(split, "domain=shop.model", "godTypeMinPackages=2").errorMessages.contains("God type"))
  }

  test("hiding the fields behind accessor methods of the class does not hide who uses what") {
    val wrapped = "Customer.scala" -> """package shop.model
case class Customer(
    id: Int, name: String, email: String,
    billingAddress: String, vatId: String, iban: String,
    shippingAddress: String, carrier: String, deliveryNotes: String,
    newsletter: Boolean, segment: String, lastCampaign: String
):
  def forBilling: String = billingAddress + vatId + iban + name
  def forShipping: String = shippingAddress + carrier + deliveryNotes + name
  def forMarketing: String = email + newsletter + segment + lastCampaign
"""
    val r = design(
      List(wrapped, billing("  def invoice(c: Customer): String = c.forBilling\n"), shipping("  def label(c: Customer): String = c.forShipping\n"),
        marketing("  def mail(c: Customer): String = c.forMarketing\n")),
      "domain=shop.model"
    )
    assert(r.errorMessages.contains("God type `shop.model.Customer`"), r.errorMessages)
    assert(r.errorMessages.contains("shop.billing: billingAddress, iban, name, vatId"), r.errorMessages)
  }

  test("a type outside the domain is not judged") {
    val r = design(split, "domain=shop.other")
    assert(!r.errorMessages.contains("God type"), r.errorMessages)
  }

  private val service = (verbs: List[String]) => List(
    "Customer.scala" -> "package shop.model\ncase class Customer(id: Int, name: String)\n",
    "CustomerService.scala" -> (
      "package shop.model\nclass CustomerService(var store: Map[Int, Customer], var log: List[String], var cache: Map[String, Int]):\n" +
        verbs.mkString("\n") + "\n"
    )
  )

  test("an entity service with create, update, delete and query for one noun in separate groups is a god type") {
    val ms = List(
      "  def createCustomer(n: String): Int = { store = store + (store.size -> Customer(store.size, n)); store.size }",
      "  def registerCustomer(n: String): Int = createCustomer(n)",
      "  def renameCustomer(id: Int, n: String): Unit = store = store.updated(id, store(id).copy(name = n))",
      "  def changeCustomerName(id: Int, n: String): Unit = renameCustomer(id, n)",
      "  def deleteCustomer(id: Int): Unit = store = store - id",
      "  def removeCustomer(id: Int): Unit = deleteCustomer(id)",
      "  def getCustomer(id: Int): Customer = store(id)",
      "  def findCustomerByName(n: String): Option[Customer] = store.values.find(_.name == n)",
      "  def listCustomers: List[Customer] = store.values.toList",
      "  def recordEvent(e: String): Unit = log = e :: log",
      "  def lastEvents: List[String] = log.take(5)",
      "  def countHits(k: String): Int = cache.getOrElse(k, 0)",
      "  def addHit(k: String): Unit = cache = cache.updated(k, countHits(k) + 1)"
    )
    val m = design(service(ms), "domain=shop.model").errorMessages
    assert(m.contains("Entity service `shop.model.CustomerService`"), m)
    assert(m.contains("create, delete, query, update"), m)
    assert(m.contains("Split by use case or workflow"), m)
  }

  test("a small service, or one whose name is no entity, is not reported") {
    val few = design(service(List("  def createCustomer(n: String): Int = 1", "  def getCustomer(id: Int): Customer = store(id)")), "domain=shop.model")
    assert(!few.errorMessages.contains("Entity service"), few.errorMessages)
  }
