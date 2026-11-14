package layers.plugin

import munit.FunSuite

class SlopTests extends FunSuite:

  private val t = Thresholds()

  private def facts(
      passThroughs: List[PassThroughFact] = Nil,
      clones: List[CloneFact] = Nil,
      typeDecls: List[TypeDecl] = Nil
  ) = FileFacts("F.scala", Set("p"), Nil, Map.empty, Nil, Nil, passThroughs, clones, typeDecls)

  private def decl(sym: String, kind: String, pkg: String = "p", parents: List[String] = Nil, sealedType: Boolean = false, generic: Boolean = false) =
    TypeDecl("F.scala", 1, sym, pkg, kind, sealedType, generic, local = false, parents)

  test("pass-through becomes a finding with a fix") {
    val fs = Slop.findings(List(facts(passThroughs = List(PassThroughFact("F.scala", 3, "p.S.find", "p.R.find")))), t)
    assertEquals(fs.map(_.key), List("passthrough p.S.find"))
    assert(fs.head.message.contains("only forwards"), fs.head.message)
    assert(fs.head.message.contains("Fix with one of"), fs.head.message)
  }

  test("methods with the same fingerprint are one clone finding") {
    val cs = List(
      CloneFact("A.scala", 1, "p.A.f", 40, "h1"),
      CloneFact("B.scala", 9, "p.B.g", 38, "h1"),
      CloneFact("C.scala", 2, "p.C.h", 40, "h2")
    )
    val fs = Slop.findings(List(facts(clones = cs)), t)
    assertEquals(fs.map(_.key), List("clone p.A.f ~ p.B.g"))
    assertEquals(fs.head.value, 40)
    assert(fs.head.message.contains("A.scala:1") && fs.head.message.contains("B.scala:9"), fs.head.message)
  }

  test("clones below the minimum size are ignored") {
    val cs = List(CloneFact("A.scala", 1, "p.A.f", 25, "h1"), CloneFact("B.scala", 9, "p.B.g", 25, "h1"))
    assertEquals(Slop.findings(List(facts(clones = cs)), t), Nil)
    assertEquals(Slop.findings(List(facts(clones = cs)), t.copy(minCloneSize = 20)).size, 1)
  }

  test("the same method seen twice is not a clone of itself") {
    val cs = List(CloneFact("A.scala", 1, "p.A.f", 40, "h1"), CloneFact("A.scala", 1, "p.A.f", 40, "h1"))
    assertEquals(Slop.findings(List(facts(clones = cs)), t), Nil)
  }

  test("trait with one implementation in the same package is flagged") {
    val ds = List(decl("p.Store", "trait"), decl("p.MemStore", "class", parents = List("p.Store")))
    assertEquals(Slop.findings(List(facts(typeDecls = ds)), t).map(_.key), List("single-impl p.Store"))
  }

  test("single implementation in another package is a boundary, not slop") {
    val ds = List(decl("p.Store", "trait"), decl("q.MemStore", "class", pkg = "q", parents = List("p.Store")))
    assertEquals(Slop.findings(List(facts(typeDecls = ds)), t), Nil)
  }

  test("several implementations, sealed, generic, or hierarchies are not flagged") {
    val two = List(
      decl("p.Store", "trait"),
      decl("p.A", "class", parents = List("p.Store")),
      decl("p.B", "object", parents = List("p.Store"))
    )
    assertEquals(Slop.findings(List(facts(typeDecls = two)), t), Nil)
    val sealedT = List(decl("p.Store", "trait", sealedType = true), decl("p.A", "class", parents = List("p.Store")))
    assertEquals(Slop.findings(List(facts(typeDecls = sealedT)), t), Nil)
    val generic = List(decl("p.Store", "trait", generic = true), decl("p.A", "class", parents = List("p.Store")))
    assertEquals(Slop.findings(List(facts(typeDecls = generic)), t), Nil)
    val hierarchy = List(
      decl("p.Store", "trait"),
      decl("p.Sub", "trait", parents = List("p.Store")),
      decl("p.A", "class", parents = List("p.Store"))
    )
    assertEquals(Slop.findings(List(facts(typeDecls = hierarchy)), t), Nil)
  }

  test("trait without implementations is not flagged here") {
    assertEquals(Slop.findings(List(facts(typeDecls = List(decl("p.Store", "trait")))), t), Nil)
  }

  test("slop facts survive the codec") {
    val f = facts(
      List(PassThroughFact("F.scala", 3, "p.S.find", "p.R.find")),
      List(CloneFact("F.scala", 4, "p.S.g", 33, "abc123")),
      List(decl("p.Store", "trait", sealedType = true, generic = true), decl("p.M", "class", parents = List("p.Store", "p.Other")))
    )
    assertEquals(FileFacts.decode(FileFacts.encode(f)).map(_.copy(source = "F.scala")), Some(f))
  }
