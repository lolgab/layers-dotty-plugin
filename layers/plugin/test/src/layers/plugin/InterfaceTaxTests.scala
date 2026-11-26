package layers.plugin

import munit.FunSuite

class InterfaceTaxTests extends FunSuite:

  private def decl(sym: String, kind: String, pkg: String, parents: List[String] = Nil, generic: Boolean = false, sealedType: Boolean = false) =
    TypeDecl(s"/src/$pkg/${sym.split('.').last}.scala", 1, sym, pkg, kind, sealedType, generic, local = false, parents)

  private def dep(from: String, to: String) =
    val fp = from.take(from.lastIndexOf('.'))
    val tp = to.take(to.lastIndexOf('.'))
    ClassDep(s"/src/$fp/${from.drop(fp.length + 1)}.scala", 3, from, to, fp, tp, 1)

  private def check(types: List[TypeDecl], deps: List[ClassDep], classes: List[ClassFact] = Nil, passes: List[PassThroughFact] = Nil) =
    val edges = deps.filter(d => d.fromPkg != d.toPkg).map(d => Edge(d.file, d.line, d.fromPkg, d.fromTop, d.toPkg, d.toTop))
    val f = FileFacts(
      "/src/All.scala", (types.map(_.pkg) ++ deps.flatMap(d => List(d.fromPkg, d.toPkg))).toSet, edges,
      classes = classes, passThroughs = passes, typeDecls = types, classDeps = deps
    )
    InterfaceTax.findings(List(f), PackageGraph.build(List(f)))

  // `service.Notifier` is a trait implemented by `infra.SmtpNotifier`, used by `app.Checkout`
  private val types = List(
    decl("service.Notifier", "trait", "service"), decl("infra.SmtpNotifier", "class", "infra", List("service.Notifier")),
    decl("app.Checkout", "class", "app")
  )
  private val uses = List(dep("app.Checkout", "service.Notifier"), dep("infra.SmtpNotifier", "service.Notifier"), dep("app.Checkout", "infra.SmtpNotifier"))

  test("a trait with one implementation elsewhere and one user that could use the class directly is an interface tax") {
    val fs = check(types, uses.take(2))
    assertEquals(fs.map(_.key), List("interface-tax service.Notifier"))
    assert(fs.head.message.contains("single implementation") && fs.head.message.contains("app.Checkout"), fs.head.message)
    assert(fs.head.message.contains("Fix with one of") && fs.head.message.contains("second implementation"), fs.head.message)
  }

  test("an interface that breaks a real dependency cycle is a boundary, not a tax") {
    // infra already depends on app (it reads Checkout's config), so app cannot depend on infra
    val withBack = uses.take(2) :+ dep("infra.SmtpNotifier", "app.Checkout")
    assertEquals(check(types, withBack), Nil)
  }

  test("two users, a second implementation, a same-package implementation or sealed/generic types are not flagged here") {
    val second = types :+ decl("app.Other", "class", "app") :+ decl("infra.Fake", "class", "infra", List("service.Notifier"))
    assertEquals(check(second, uses.take(2)), Nil)
    val twoUsers = types :+ decl("app.Cart", "class", "app")
    assertEquals(check(twoUsers, uses.take(2) :+ dep("app.Cart", "service.Notifier")), Nil)
    val samePkg = List(decl("service.Notifier", "trait", "service"), decl("service.SmtpNotifier", "class", "service", List("service.Notifier")))
    assertEquals(check(samePkg, Nil), Nil) // the single-impl finding covers it
    assertEquals(check(List(types.head.copy(generic = true), types(1), types(2)), uses.take(2)), Nil)
    assertEquals(check(List(types.head.copy(sealedType = true), types(1), types(2)), uses.take(2)), Nil)
  }

  test("a trait that nothing else uses is left alone: it may be an extension point for code outside the module") {
    assertEquals(check(types.take(2), uses.take(1).tail), Nil)
  }

  test("a second implementation that nothing uses still counts as a second implementation: it is not flagged") {
    // documented gap: only the number of implementations is checked, the message says a stub does not count
    val fakeOnly = types :+ decl("infra.Noop", "object", "infra", List("service.Notifier"))
    assertEquals(check(fakeOnly, uses.take(2)), Nil)
  }

  private def classFact(sym: String, methods: String*) =
    ClassFact(s"/src/${sym.replace('.', '/')}.scala", 1, sym, methods.toList.map(m => ClassMethod(m, Nil, Nil, Nil)))

  private def passes(owner: String, target: String, names: String*) =
    names.toList.map(n => PassThroughFact(s"/src/x.scala", 2, s"$owner.$n", s"$target.$n"))

  test("a class whose every method forwards to one other class and that has one caller is a forwarder") {
    val c = classFact("app.UserRepo", "find", "save", "delete")
    val fs = check(Nil, List(dep("app.Service", "app.UserRepo")), List(c), passes("app.UserRepo", "db.UserTable", "find", "save", "delete"))
    assertEquals(fs.map(_.key), List("forwarder-class app.UserRepo"))
    assertEquals(fs.head.value, 3)
    assert(fs.head.message.contains("only caller is `app.Service`") && fs.head.message.contains("db.UserTable"), fs.head.message)
  }

  test("a class with real behaviour in one method, several callers, or several targets is not a forwarder") {
    val c = classFact("app.UserRepo", "find", "save", "delete")
    val p = passes("app.UserRepo", "db.UserTable", "find", "save") // delete does more
    assertEquals(check(Nil, List(dep("app.Service", "app.UserRepo")), List(c), p), Nil)
    val all = passes("app.UserRepo", "db.UserTable", "find", "save", "delete")
    assertEquals(check(Nil, List(dep("app.A", "app.UserRepo"), dep("app.B", "app.UserRepo")), List(c), all), Nil)
    val mixed = passes("app.UserRepo", "db.UserTable", "find", "save") ++ passes("app.UserRepo", "db.Other", "delete")
    assertEquals(check(Nil, List(dep("app.Service", "app.UserRepo")), List(c), mixed), Nil)
  }

  test("a single forwarding method is left to the pass-through finding") {
    val c = classFact("app.UserRepo", "find")
    assertEquals(check(Nil, Nil, List(c), passes("app.UserRepo", "db.UserTable", "find")), Nil)
  }
