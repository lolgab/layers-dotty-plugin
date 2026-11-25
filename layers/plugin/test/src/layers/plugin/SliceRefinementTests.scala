package layers.plugin

import munit.FunSuite

import ArchTestSupport.design

/** A hub is a grab-bag by who owns its types, not by size; the application root may name the slices it wires. */
class SliceRefinementTests extends FunSuite:

  private def decl(pkg: String, name: String) =
    TypeDecl(s"/src/$pkg/$name.scala", 1, s"$pkg.$name", pkg, "class", false, false, false, Nil)

  private def dep(from: String, to: String, line: Int = 1) =
    val (fp, ft) = from.splitAt(from.lastIndexOf('.'))
    val (tp, tt) = to.splitAt(to.lastIndexOf('.'))
    ClassDep(s"/src/$fp/${ft.drop(1)}.scala", line, from, to, fp, tp, 1)

  private def facts(types: List[TypeDecl], deps: List[ClassDep], arch: List[ArchFact] = Nil) =
    val edges = deps.filter(d => d.fromPkg != d.toPkg).map(d => Edge(d.file, d.line, d.fromPkg, d.fromTop, d.toPkg, d.toTop))
    val pkgs = (types.map(_.pkg) ++ deps.flatMap(d => List(d.fromPkg, d.toPkg))).toSet
    FileFacts("/src/All.scala", pkgs, edges, typeDecls = types, classDeps = deps, arch = arch)

  private def check(f: FileFacts, opts: String*) =
    SliceChecks.findings(List(f), PackageGraph.build(List(f)), opts.toList)

  private val users = 12

  /** `domain` with `n` types; `usedBy(i)` lists the users (1..12) of type i. */
  private def hub(n: Int, usedBy: Int => List[Int], extra: List[ClassDep] = Nil, extraTypes: List[TypeDecl] = Nil) =
    val types = (1 to n).map(i => decl("domain", s"K$i")).toList ++ (1 to users).map(u => decl(s"u$u", s"U$u")) ++ extraTypes
    val deps = (for i <- 1 to n; u <- usedBy(i) yield dep(s"u$u.U$u", s"domain.K$i")).toList ++ extra
    facts(types, deps)

  test("a big domain model that the slices use together is not a grab-bag") {
    // 100 types, every one used by two or three slices
    val f = hub(100, i => List(1 + i % users, 1 + (i + 3) % users))
    assertEquals(check(f).filter(_.kind == "slice-hub-grabbag"), Nil)
  }

  test("a big hub whose types belong to single slices is a grab-bag, listed by owning slice") {
    // 70 types: 50 used by one slice each, 20 by everyone
    val f = hub(70, i => if i <= 50 then List(1 + i % users) else (1 to users).toList)
    val g = check(f).filter(_.kind == "slice-hub-grabbag")
    assertEquals(g.map(_.key), List("slice-hub-grabbag domain"))
    val m = g.head.message
    assert(m.contains("71% of them (limit 40%)") || m.contains("% of them (limit 40%)"), m)
    assert(m.contains("move to `u1`") && m.contains("move to `u2`"), m)
    assert(m.contains("50 of the 70 types are used by one slice only"), m)
    // below the size floor it is not judged, and the share is an option
    assertEquals(check(hub(50, i => List(1 + i % users))).filter(_.kind == "slice-hub-grabbag"), Nil)
    assertEquals(check(f, "hubOwnedShare=90").filter(_.kind == "slice-hub-grabbag"), Nil)
    assertEquals(check(f, "maxHubTypes=80").filter(_.kind == "slice-hub-grabbag"), Nil)
  }

  test("a type that cannot move without a package cycle counts as shared") {
    // 61 types, nothing else uses them; K1 is used by u1 only but K2 (shared) uses K1, and K3 needs y which needs u1
    val extra = List(dep("domain.K2", "domain.K1"), dep("domain.K3", "y.Y"), dep("y.Y", "u1.U1"))
    val used = (i: Int) => if i == 1 || i == 3 then List(1) else if i == 2 then List(1, 2) else if i == 4 then (1 to users).toList else Nil
    val f = hub(61, used, extra, List(decl("y", "Y")))
    val g = check(f).filter(_.kind == "slice-hub-grabbag").head.message
    assert(g.contains("2 more types have a single user slice but cannot move alone"), g)
    assert(!g.contains("move to `u1`"), g)
    // the same types with no obstacle move
    val free = check(hub(61, used)).filter(_.kind == "slice-hub-grabbag").head.message
    assert(free.contains("move to `u1` (2): K1, K3"), free)
  }

  test("ownership percentage counts movable and unused types") {
    val f = hub(70, i => if i <= 50 then List(1 + i % users) else Nil)
    val own = SliceCoupling.hubOwnership(new Slices(PackageGraph.build(List(f)), List(f), SliceOptions()), "domain", f.typeDecls.filter(_.pkg == "domain"))
    assertEquals((own.owned.values.map(_.size).sum, own.unused, own.blocked, own.percent), (50, 20, 0, 100))
  }

  private def apps(extra: List[ArchFact] = Nil, caller: Boolean = false) =
    val ts = (1 to 8).map(i => decl(s"app.t$i", s"T$i")).toList
    val deps = (1 to 8).map(i => dep("app.Main", s"app.t$i.T$i")).toList ++ (if caller then List(dep("zz.Z", "app.Main")) else Nil)
    facts(decl("app", "Main") :: (if caller then List(decl("zz", "Z")) else Nil) ++ ts, deps, extra)

  private val entry = List(ArchFact("entry", "/src/All.scala", 1, List("app")))

  test("the application root that holds the entry point may depend on many slices") {
    assertEquals(check(apps()).filter(_.kind == "slice-fanout").map(_.key), List("slice-fanout app"))
    assertEquals(check(apps(entry)).filter(_.kind == "slice-fanout"), Nil)
    assertEquals(check(apps(), "entry=app").filter(_.kind == "slice-fanout"), Nil)
    // something depends on it: it is not a root
    assertEquals(check(apps(entry, caller = true)).filter(_.kind == "slice-fanout").map(_.key), List("slice-fanout app"))
    // an entry point elsewhere does not exempt it
    assertEquals(check(apps(List(ArchFact("entry", "/src/All.scala", 1, List("other"))))).filter(_.kind == "slice-fanout").map(_.key), List("slice-fanout app"))
  }

  private def rootProject(entryCode: String) =
    val n = 4
    val targets = (1 to n).toList.map(i => s"T$i.scala" -> s"package t$i\nobject layer\nobject T$i { def v: Int = $i }\n")
    val body = (1 to n).map(i => s"t$i.T$i.v").mkString(" + ")
    ("Hub.scala" ->
      s"""package hub
@layers.dependsOnPackages(${(1 to n).map(i => "\"t" + i + "\"").mkString(", ")})
object layer
object Hub { def all: Int = $body }
$entryCode
""") :: targets

  private def fanout(entryCode: String, opts: String*) =
    CompilerPluginTestHelper.compile(rootProject(entryCode), ArchTestSupport.pluginJar, "design" :: "maxSliceFanOut=2" :: opts.toList).errorMessages

  test("entry points are found: @main, def main and extends App") {
    assert(fanout("").contains("Slice `hub` depends on 4 other slices"))
    assert(!fanout("@main def start(): Unit = println(Hub.all)").contains("Slice `hub`"))
    assert(!fanout("object Launcher { def main(args: Array[String]): Unit = println(Hub.all) }").contains("Slice `hub`"))
    assert(!fanout("object Launcher extends App { println(Hub.all) }").contains("Slice `hub`"))
    assert(!fanout("", "entry=hub").contains("Slice `hub`"))
  }

  test("a cohesion finding on top-level functions says where extensions belong") {
    val shapes = (1 to 5).map(i => s"case class A$i(n: Int)").mkString("\n")
    val defs = (1 to 5).flatMap(i => List(s"def f$i(a: A$i): Int = a.n", s"def g$i(a: A$i): Int = a.n + 1")).mkString("\n")
    val r = ArchTestSupport.design(List("Layer.scala" -> "package p\nobject layer\n", "Src.scala" -> s"package p\n$shapes\n$defs\n"))
    assert(r.errorMessages.contains("put each extension next to the type it extends"), r.errorMessages)
    assert(r.errorMessages.contains("one `extensions` object per receiver family"), r.errorMessages)
  }
