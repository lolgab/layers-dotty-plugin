package layers.plugin

import munit.FunSuite

class LayerTraitClassTests extends FunSuite:

  private def pluginJarPath: String =
    Option(System.getProperty("layers.plugin.jar")).getOrElse(
      sys.error("layers.plugin.jar system property not set. Tests must run with forkArgs from build.")
    )

  test("plugin compiles layer with trait and class extending that trait") {
    val sources = List(
      "DomainLayer.scala" -> """package domain
object layer
""",
      "DomainUserRepo.scala" -> """package domain
trait UserRepo:
  def find(id: String): Option[String]
""",
      "DomainInMemoryUserRepo.scala" -> """package domain
class InMemoryUserRepo extends UserRepo:
  def find(id: String): Option[String] = None
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(!result.hasErrors, s"Expected compilation to succeed. Errors: ${result.errorMessages}")
  }
