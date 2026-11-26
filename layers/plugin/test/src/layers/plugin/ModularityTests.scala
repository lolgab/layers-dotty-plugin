package layers.plugin

import munit.FunSuite

class ModularityTests extends FunSuite:

  private def edges(pairs: (Int, Int)*): Map[(Int, Int), Double] = pairs.map(p => p -> 1.0).toMap

  test("two cliques joined by one edge are two communities") {
    val c = Modularity.communities(6, edges(0 -> 1, 0 -> 2, 1 -> 2, 3 -> 4, 3 -> 5, 4 -> 5, 2 -> 3))
    assertEquals(Set(c(0), c(1), c(2)).size, 1)
    assertEquals(Set(c(3), c(4), c(5)).size, 1)
    assertNotEquals(c(0), c(3))
  }

  test("a complete graph is one community") {
    val c = Modularity.communities(4, edges(0 -> 1, 0 -> 2, 0 -> 3, 1 -> 2, 1 -> 3, 2 -> 3))
    assertEquals(c.toSet.size, 1)
  }

  test("nodes without edges stay alone and the graph may be empty") {
    assertEquals(Modularity.communities(3, Map.empty).toSet.size, 3)
    assertEquals(Modularity.communities(0, Map.empty).length, 0)
  }

  test("the result is deterministic") {
    val g = edges(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 0, 4 -> 5)
    assertEquals(Modularity.communities(6, g).toList, Modularity.communities(6, g).toList)
  }
