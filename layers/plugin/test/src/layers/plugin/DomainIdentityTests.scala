package layers.plugin

import munit.FunSuite

import ArchTestSupport.design

class DomainIdentityTests extends FunSuite:

  private val user = "User.scala" -> "package shop.model\ncase class User(id: Int, name: String)\n"
  private val doc = "Doc.scala" -> "package shop.model\ncase class Doc(id: Int, ownerId: Int)\n"

  private def domainWith(body: String) = List(user, doc, "Rules.scala" -> s"package shop.model\n$body\n")

  test("a domain method that takes the acting user is reported with the principle and fixes") {
    val r = design(domainWith("object Rules:\n  def edit(user: User, d: Doc): Doc = d\n"), "domain=shop.model")
    val m = r.errorMessages
    assert(m.contains("Domain code `shop.model.Rules.edit` takes the acting identity or request context"), m)
    assert(m.contains("parameter `user: User`") && m.contains("is the acting user"), m)
    assert(m.contains("authorization (who may do it) belongs in the application layer"), m)
    assert(m.contains("Fix with one of"), m)
  }

  test("a domain User that is only data is not the acting identity") {
    val r = design(domainWith("case class Note(author: User, text: String)\nobject Rules:\n  def describe(owner: User, d: Doc): String = owner.name + d.id\n"), "domain=shop.model")
    assert(!r.errorMessages.contains("acting identity"), r.errorMessages)
  }

  test("comparing a user parameter with ownership data is a permission check, whatever the parameter is called") {
    val r = design(domainWith("object Rules:\n  def mayEdit(u: User, d: Doc): Boolean = d.ownerId == u.id\n"), "domain=shop.model")
    assert(r.errorMessages.contains("is compared with ownership data to decide who may do it"), r.errorMessages)
  }

  test("passing only the id to compare is the fix") {
    val r = design(domainWith("object Rules:\n  def mayEdit(userId: Int, d: Doc): Boolean = d.ownerId == userId\n"), "domain=shop.model")
    assert(!r.errorMessages.contains("acting identity"), r.errorMessages)
  }

  test("a session, request or context type from outside the domain is the identity, as a parameter or a using clause") {
    val sources = List(
      "Auth.scala" -> "package shop.auth\nclass Session(val user: String)\n",
      "Web.scala" -> "package shop.web\nclass RequestContext(val path: String)\n",
      "Rules.scala" -> "package shop.model\nobject Rules:\n  def a(s: shop.auth.Session): Int = 1\n  def b(x: Int)(using c: shop.web.RequestContext): Int = x\n"
    )
    val m = design(sources, "domain=shop.model").errorMessages
    assert(m.contains("`shop.model.Rules.a`") && m.contains("`Session` is a request or identity type"), m)
    assert(m.contains("`shop.model.Rules.b`") && m.contains("(using)"), m)
  }

  test("a domain type that happens to be called Context is a domain concept") {
    val r = design(domainWith("case class ValidationContext(strict: Boolean)\nobject Rules:\n  def check(c: ValidationContext, d: Doc): Boolean = c.strict\n"), "domain=shop.model")
    assert(!r.errorMessages.contains("acting identity"), r.errorMessages)
  }

  test("a class that holds the acting user as a field is reported, one that holds an owner is not") {
    val bad = design(domainWith("class Editor(val actor: User):\n  def name = actor.name\n"), "domain=shop.model")
    assert(bad.errorMessages.contains("field `actor: User`"), bad.errorMessages)
    val ok = design(domainWith("class Lot(val owner: User):\n  def name = owner.name\n"), "domain=shop.model")
    assert(!ok.errorMessages.contains("acting identity"), ok.errorMessages)
  }

  test("the same method outside the domain is fine: that is where authorization lives") {
    val r = design(List(user, "Handler.scala" -> "package shop.web\nimport shop.model.*\nobject Handler:\n  def edit(user: User, d: Doc): Doc = d\n"), "domain=shop.model")
    assert(!r.errorMessages.contains("acting identity"), r.errorMessages)
  }

  test("identityTypes replaces the built-in vocabulary") {
    val sources = domainWith("case class Tenant(id: Int)\nobject Rules:\n  def a(t: Tenant): Int = t.id\n  def b(user: User): Int = user.id\n")
    val m = design(sources, "domain=shop.model", "identityTypes=Tenant").errorMessages
    assert(m.contains("`shop.model.Rules.a`"), m)
    assert(!m.contains("`shop.model.Rules.b`"), m)
  }

  test("the baseline accepts a legacy finding") {
    val file = java.nio.file.Files.createTempFile("arch-baseline", ".txt")
    java.nio.file.Files.delete(file)
    design(domainWith("object Rules:\n  def edit(user: User, d: Doc): Doc = d\n"), "domain=shop.model", s"baseline=$file", "updateBaseline")
    assert(java.nio.file.Files.readString(file).contains("domain-identity shop.model.Rules$.edit = 1"), java.nio.file.Files.readString(file))
    val again = design(domainWith("object Rules:\n  def edit(user: User, d: Doc): Doc = d\n"), "domain=shop.model", s"baseline=$file")
    assert(!again.errorMessages.contains("acting identity"), again.errorMessages)
  }
