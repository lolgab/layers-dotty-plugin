package layers.plugin

import munit.FunSuite

class RatchetTests extends FunSuite:

  private val changes = GitSignals.Changes(Map("/r/A.scala" -> List((10, 12))), Set("/r/New.scala"))

  private def f(key: String, file: String, line: Int, ranges: List[(Int, Int)] = Nil) = Finding(key, 1, file, line, "m", ranges)

  test("method findings in changed code fail; the rest are only reported") {
    val touched = f("cyclomatic a.A.run", "/r/A.scala", 8, List((8, 11)))
    val untouched = f("cyclomatic a.A.other", "/r/A.scala", 40, List((40, 60)))
    val other = f("lines a.B.go", "/r/B.scala", 1)
    val (fail, existing) = Ratchet.split(List(touched, untouched, other), changes)
    assertEquals(fail, List(touched))
    assertEquals(existing, List(untouched, other))
  }

  test("a helper counted with its caller makes the caller touched") {
    val withHelper = f("cognitive a.A.run", "/r/A.scala", 1, List((1, 5), (11, 12)))
    assertEquals(Ratchet.split(List(withHelper), changes)._1, List(withHelper))
  }

  test("class findings fail when the declaration itself is new or changed") {
    val god = f("cohesion a.God", "/r/A.scala", 30)
    val fresh = f("cohesion a.Fresh", "/r/New.scala", 3)
    val edited = f("facade a.Edited", "/r/A.scala", 11)
    val (fail, existing) = Ratchet.split(List(god, fresh, edited), changes)
    assertEquals(fail.map(_.key), List("cohesion a.Fresh", "facade a.Edited"))
    assertEquals(existing, List(god))
  }

  test("project-wide stability findings always fail") {
    val sdp = f("sdp a -> b", "/r/Z.scala", 1)
    assertEquals(Ratchet.split(List(sdp), changes)._1, List(sdp))
  }

  test("the summary says what was left alone") {
    val s = Ratchet.summary(List(f("lines a.x", "F", 1), f("lines a.y", "F", 2), f("cohesion a.Z", "F", 3)), "main")
    assert(s.contains("3 existing findings") && s.contains("2 lines, 1 cohesion") && s.contains("main"), s)
  }
