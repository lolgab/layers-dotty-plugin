package layers.plugin

import munit.FunSuite

class SharedOwnershipTests extends FunSuite:

  private def decl(pkg: String, name: String) =
    TypeDecl(s"/src/$pkg/$name.scala", 1, s"$pkg.$name", pkg, "class", false, false, false, Nil)

  private def dep(from: String, to: String) =
    val (fp, _) = from.splitAt(from.lastIndexOf('.'))
    val (tp, _) = to.splitAt(to.lastIndexOf('.'))
    ClassDep(s"/src/$fp/${from.drop(fp.length + 1)}.scala", 1, from, to, fp, tp, 1)

  private def check(types: List[TypeDecl], deps: List[ClassDep], opts: String*) =
    val edges = deps.filter(d => d.fromPkg != d.toPkg).map(d => Edge(d.file, d.line, d.fromPkg, d.fromTop, d.toPkg, d.toTop))
    val f = FileFacts("/src/All.scala", (types.map(_.pkg) ++ deps.flatMap(d => List(d.fromPkg, d.toPkg))).toSet, edges, typeDecls = types, classDeps = deps)
    SharedOwnership.findings(new Slices(PackageGraph.build(List(f)), List(f), SliceOptions.fromOptions(opts.toList)))

  private val types = List(
    decl("util", "Money"), decl("billing", "Invoice"), decl("billing", "Payment"), decl("reporting", "Report"), decl("shipping", "Parcel")
  )

  test("a shared type that every user uses together with one slice belongs to that slice") {
    val deps = List(
      dep("billing.Invoice", "util.Money"), dep("reporting.Report", "util.Money"), dep("reporting.Report", "billing.Invoice")
    )
    val fs = check(types, deps)
    assertEquals(fs.map(_.key), List("shared-owned util.Money"))
    assert(fs.head.message.contains("belongs to slice `billing`") && fs.head.message.contains("no dependency is added"), fs.head.message)
    assertEquals(fs.head.value, 2)
  }

  test("a shared type used independently by slices stays shared") {
    val deps = List(dep("billing.Invoice", "util.Money"), dep("shipping.Parcel", "util.Money"), dep("billing.Payment", "util.Money"))
    assertEquals(check(types, deps), Nil)
  }

  test("a type with a single user, or users in one slice, is not judged here") {
    assertEquals(check(types, List(dep("billing.Invoice", "util.Money"))), Nil)
    assertEquals(check(types, List(dep("billing.Invoice", "util.Money"), dep("billing.Payment", "util.Money"))), Nil)
  }

  test("the owner must not be a hub, and moving must not create a cycle") {
    val many = (1 to 12).map(i => decl(s"s$i", s"S$i")).toList
    val hubUse = many.map(s => dep(s"${s.pkg}.${s.sym.split('.').last}", "billing.Invoice"))
    val deps = List(dep("billing.Invoice", "util.Money"), dep("reporting.Report", "util.Money"), dep("reporting.Report", "billing.Invoice")) ++ hubUse
    assertEquals(check(types ++ many, deps), Nil)
    // Money itself uses a type of reporting, which uses billing: it would drag billing into a cycle
    val cyc = List(
      dep("billing.Invoice", "util.Money"), dep("reporting.Report", "util.Money"), dep("reporting.Report", "billing.Invoice"),
      dep("util.Money", "reporting.Report")
    )
    assertEquals(check(types, cyc), Nil)
  }

  test("only types in shared packages are judged; the shared names are an option") {
    val ts = List(decl("lib", "Money"), decl("billing", "Invoice"), decl("reporting", "Report"))
    val deps = List(dep("billing.Invoice", "lib.Money"), dep("reporting.Report", "lib.Money"), dep("reporting.Report", "billing.Invoice"))
    assertEquals(check(ts, deps).map(_.key), List("shared-owned lib.Money")) // `lib` is a shared name
    val ts2 = List(decl("money", "Money"), decl("billing", "Invoice"), decl("reporting", "Report"))
    val deps2 = List(dep("billing.Invoice", "money.Money"), dep("reporting.Report", "money.Money"), dep("reporting.Report", "billing.Invoice"))
    assertEquals(check(ts2, deps2), Nil) // `money` is a slice
    assertEquals(check(ts2, deps2, "shared=money").map(_.key), List("shared-owned money.Money"))
  }

  test("adding an unrelated second owner's use makes the type genuinely shared") {
    val deps = List(
      dep("billing.Invoice", "util.Money"), dep("reporting.Report", "util.Money"), dep("reporting.Report", "billing.Invoice"),
      dep("shipping.Parcel", "util.Money")
    )
    assertEquals(check(types, deps), Nil)
  }
