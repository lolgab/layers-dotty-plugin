package layers.plugin

import java.nio.file.Path

import munit.FunSuite

class GitSignalsTests extends FunSuite:

  private val root = Path.of("/repo")

  test("diff hunks become line ranges of the new file") {
    val diff =
      """diff --git a/src/A.scala b/src/A.scala
        |--- a/src/A.scala
        |+++ b/src/A.scala
        |@@ -3,2 +3,4 @@ class A
        |+x
        |@@ -20 +22 @@
        |+y
        |@@ -30,2 +31,0 @@
        |-gone
        |diff --git a/src/Old.scala b/src/Old.scala
        |--- a/src/Old.scala
        |+++ /dev/null
        |@@ -1,3 +0,0 @@
        |""".stripMargin
    val parsed = GitSignals.parseDiff(diff, root)
    assertEquals(parsed, Map("/repo/src/A.scala" -> List((3, 6), (22, 22), (31, 31))))
  }

  test("changes touch a file where a range overlaps, or everywhere in an untracked file") {
    val c = GitSignals.Changes(Map("/repo/A.scala" -> List((10, 12))), Set("/repo/New.scala"))
    assert(c.touches("/repo/A.scala", List((12, 20))))
    assert(c.touches("/repo/A.scala", List((1, 1), (9, 10))))
    assert(!c.touches("/repo/A.scala", List((13, 20))))
    assert(!c.touches("/repo/B.scala", List((1, 100))))
    assert(c.touches("/repo/New.scala", List((500, 501))))
  }

  test("the log is one file set per commit") {
    val log = "\u0000\na.scala\nb.scala\n\u0000\nb.scala\n\u0000\nc.scala\na.scala"
    assertEquals(GitSignals.parseLog(log), List(Set("a.scala", "b.scala"), Set("b.scala"), Set("c.scala", "a.scala")))
    assertEquals(GitSignals.churn(GitSignals.parseLog(log)), Map("a.scala" -> 2, "b.scala" -> 2, "c.scala" -> 1))
  }

  test("files that keep changing together are coupled; mass commits and rare pairs are not") {
    val together = List.fill(5)(Set("a", "b"))
    val apart = List(Set("a"), Set("c"), Set("c", "d"))
    val mass = List(Set((1 to 40).map(i => s"f$i")*) + "a" + "c")
    val cs = GitSignals.coChange(together ++ apart ++ mass)
    assertEquals(cs.map(c => (c.a, c.b, c.together)), List(("a", "b", 5)))
  }

  test("hotspots rank churn times complexity and skip simple or unchanged files") {
    val hs = GitSignals.hotspots(Map("a" -> 10, "b" -> 2, "c" -> 50), Map("a" -> 30, "b" -> 100, "c" -> 0, "d" -> 40))
    assertEquals(hs.map(_.file), List("a", "b"))
    assertEquals(hs.head.score, 300)
  }
