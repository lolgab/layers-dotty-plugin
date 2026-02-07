package layers.plugin

import munit.FunSuite

class CycleDetectionTests extends FunSuite:

  private def pluginJarPath: String =
    Option(System.getProperty("layers.plugin.jar")).getOrElse(
      sys.error("layers.plugin.jar system property not set. Tests must run with forkArgs from build.")
    )

  test("cycle between files fails compilation") {
    val sources = List(
      "User.scala" -> """package core
case class User(id: String, bar: Bar)
""",
      "Bar.scala" -> """package core
case class Bar(user: User)
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(result.hasErrors, s"Expected compilation to fail, but it succeeded. Errors: ${result.errorMessages}")
    assert(
      result.errorMessages.contains("Cycle between files"),
      s"Expected 'Cycle between files' in output. Output: ${result.errorMessages}"
    )
  }

  test("one trait per package compiles successfully") {
    val sources = List(
      "UserRepo.scala" -> """package core.user
trait UserRepo:
  def find(id: String): Option[String]
""",
      "BarRepo.scala" -> """package core.bar
trait BarRepo:
  def find(id: String): Option[String]
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(!result.hasErrors, s"Expected compilation to succeed. Errors: ${result.errorMessages}")
  }

  test("package without @dependsOn cannot depend on another package") {
    val sources = List(
      "User.scala" -> """package domain
case class User(id: String)
""",
      "Service.scala" -> """package application
class Service(u: domain.User)
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(result.hasErrors, s"Expected compilation to fail. Output: ${result.errorMessages}")
    assert(
      result.errorMessages.contains("Package application cannot depend on domain"),
      s"Expected dependency error. Output: ${result.errorMessages}"
    )
  }

  test("@dependsOn in separate file is found and allows dependency") {
    val sources = List(
      "DomainUser.scala" -> """package domain
case class User(id: String)
""",
      "ApplicationLayer.scala" -> """package application
@layers.dependsOn("domain")
object layer
""",
      "ApplicationService.scala" -> """package application
class Service(u: domain.User)
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(!result.hasErrors, s"Expected compilation to succeed. Errors: ${result.errorMessages}")
  }

  test("package with layer object (no @dependsOn) compiles") {
    val sources = List(
      "Layer.scala" -> """package app
object layer
""",
      "Service.scala" -> """package app
class Service
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(!result.hasErrors, s"Expected compilation to succeed. Errors: ${result.errorMessages}")
  }

  test("maxLayers limits number of layers - fails when exceeded") {
    val sources = List(
      "DomainLayer.scala" -> """package domain
@layers.dependsOn()
object layer
""",
      "ApplicationLayer.scala" -> """package application
@layers.dependsOn("domain")
object layer
""",
      "InfrastructureLayer.scala" -> """package infrastructure
@layers.dependsOn("domain", "application")
object layer
""",
      "DomainUser.scala" -> """package domain
case class User(id: String)
""",
      "ApplicationService.scala" -> """package application
class Service(u: domain.User)
""",
      "InfrastructureRepo.scala" -> """package infrastructure
class Repo(u: domain.User, s: application.Service)
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath, "maxLayers=2")
    assert(result.hasErrors, s"Expected compilation to fail with maxLayers=2 and 3 layers. Output: ${result.errorMessages}")
    assert(
      result.errorMessages.contains("maxLayers") && result.errorMessages.contains("3 layers"),
      s"Expected maxLayers error message. Output: ${result.errorMessages}"
    )
  }

  test("@dependsOn on class fails") {
    val sources = List(
      "Wrong.scala" -> """package app
@layers.dependsOn("domain")
class Foo
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(result.hasErrors, s"Expected compilation to fail. Output: ${result.errorMessages}")
    assert(
      result.errorMessages.contains("@dependsOn may only be placed on `object layer`"),
      s"Expected @dependsOn placement error. Output: ${result.errorMessages}"
    )
  }

  test("@dependsOn on trait fails") {
    val sources = List(
      "Wrong.scala" -> """package app
@layers.dependsOn("domain")
trait Foo
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(result.hasErrors, s"Expected compilation to fail. Output: ${result.errorMessages}")
    assert(
      result.errorMessages.contains("@dependsOn may only be placed on `object layer`"),
      s"Expected @dependsOn placement error. Output: ${result.errorMessages}"
    )
  }

  test("@dependsOn on object with wrong name fails") {
    val sources = List(
      "Wrong.scala" -> """package app
@layers.dependsOn("domain")
object config
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(result.hasErrors, s"Expected compilation to fail. Output: ${result.errorMessages}")
    assert(
      result.errorMessages.contains("@dependsOn may only be placed on `object layer`"),
      s"Expected @dependsOn placement error. Output: ${result.errorMessages}"
    )
  }

  test("maxLayers allows compilation when within limit") {
    val sources = List(
      "DomainLayer.scala" -> """package domain
@layers.dependsOn()
object layer
""",
      "ApplicationLayer.scala" -> """package application
@layers.dependsOn("domain")
object layer
""",
      "DomainUser.scala" -> """package domain
case class User(id: String)
""",
      "ApplicationService.scala" -> """package application
class Service(u: domain.User)
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath, "maxLayers=3")
    assert(!result.hasErrors, s"Expected compilation to succeed with maxLayers=3 and 2 layers. Errors: ${result.errorMessages}")
  }
