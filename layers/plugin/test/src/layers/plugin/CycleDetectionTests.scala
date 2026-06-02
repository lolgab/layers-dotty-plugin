package layers.plugin

import munit.FunSuite

class CycleDetectionTests extends FunSuite:

  private def pluginJarPath: String =
    Option(System.getProperty("layers.plugin.jar")).getOrElse(
      sys.error("layers.plugin.jar system property not set. Tests must run with forkArgs from build.")
    )

  test("cycle in layer dependencies reports compiler error with full path") {
    val sources = List(
      "AppLayer.scala" -> """package app
@layers.dependsOnPackages("app.auth")
object layer
""",
      "AuthLayer.scala" -> """package app
package auth

@layers.dependsOnPackages("app")
object layer
""",
      "AuthService.scala" -> """package app
package auth

class AuthService
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(result.hasErrors, s"Expected compilation to fail with cycle, but succeeded. Errors: ${result.errorMessages}")
    assert(
      result.errorMessages.contains("Cycle in layer dependencies") && result.errorMessages.contains(" → "),
      s"Expected 'Cycle in layer dependencies' with cycle path (→) in output. Output: ${result.errorMessages}"
    )
    assert(
      result.errorMessages.contains("app") && result.errorMessages.contains("app.auth"),
      s"Expected cycle to show app and app.auth. Output: ${result.errorMessages}"
    )
  }

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

  test("package without layer dependency annotations cannot depend on another package") {
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

  test("@dependsOnPackages in separate file is found and allows dependency") {
    val sources = List(
      "DomainUser.scala" -> """package domain
case class User(id: String)
""",
      "ApplicationLayer.scala" -> """package application
@layers.dependsOnPackages("domain")
object layer
""",
      "ApplicationService.scala" -> """package application
class Service(u: domain.User)
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(!result.hasErrors, s"Expected compilation to succeed. Errors: ${result.errorMessages}")
  }

  test("@dependsOnLayers reads package from referenced layer object") {
    val sources = List(
      "DomainUser.scala" -> """package domain
case class User(id: String)
""",
      "DomainLayer.scala" -> """package domain
object layer
""",
      "ApplicationLayer.scala" -> """package application
@layers.dependsOnLayers(domain.layer)
object layer
""",
      "ApplicationService.scala" -> """package application
class Service(u: domain.User)
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(!result.hasErrors, s"Expected compilation to succeed. Errors: ${result.errorMessages}")
  }

  test("package with layer object (no dependency annotations) compiles") {
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

  test("package with no layer object uses hash_nolayers") {
    val sources = List(
      "Service.scala" -> """package app
class Service(s: String)
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(!result.hasErrors, s"Expected compilation to succeed. Errors: ${result.errorMessages}")
  }

  test("maxLayers limits number of layers - fails when exceeded") {
    val sources = List(
      "DomainLayer.scala" -> """package domain
@layers.dependsOnPackages()
object layer
""",
      "ApplicationLayer.scala" -> """package application
@layers.dependsOnPackages("domain")
object layer
""",
      "InfrastructureLayer.scala" -> """package infrastructure
@layers.dependsOnPackages("domain", "application")
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

  test("@dependsOnPackages on class fails") {
    val sources = List(
      "Wrong.scala" -> """package app
@layers.dependsOnPackages("domain")
class Foo
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(result.hasErrors, s"Expected compilation to fail. Output: ${result.errorMessages}")
    assert(
      result.errorMessages.contains("@dependsOnPackages/@dependsOnLayers may only be placed on `object layer`"),
      s"Expected layer dependency annotation placement error. Output: ${result.errorMessages}"
    )
  }

  test("@dependsOnPackages on trait fails") {
    val sources = List(
      "Wrong.scala" -> """package app
@layers.dependsOnPackages("domain")
trait Foo
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(result.hasErrors, s"Expected compilation to fail. Output: ${result.errorMessages}")
    assert(
      result.errorMessages.contains("@dependsOnPackages/@dependsOnLayers may only be placed on `object layer`"),
      s"Expected layer dependency annotation placement error. Output: ${result.errorMessages}"
    )
  }

  test("@dependsOnPackages on object with wrong name fails") {
    val sources = List(
      "Wrong.scala" -> """package app
@layers.dependsOnPackages("domain")
object config
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(result.hasErrors, s"Expected compilation to fail. Output: ${result.errorMessages}")
    assert(
      result.errorMessages.contains("@dependsOnPackages/@dependsOnLayers may only be placed on `object layer`"),
      s"Expected layer dependency annotation placement error. Output: ${result.errorMessages}"
    )
  }

  test("maxLayers allows compilation when within limit") {
    val sources = List(
      "DomainLayer.scala" -> """package domain
@layers.dependsOnPackages()
object layer
""",
      "ApplicationLayer.scala" -> """package application
@layers.dependsOnPackages("domain")
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

  test("maxLayers with single package (depth 1) passes") {
    val sources = List(
      "Layer.scala" -> """package app
@layers.dependsOnPackages()
object layer
""",
      "Service.scala" -> """package app
class Service
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath, "maxLayers=1")
    assert(!result.hasErrors, s"Expected compilation to succeed with maxLayers=1 and depth 1. Errors: ${result.errorMessages}")
  }

  test("maxLayers counts tree height (longest path), not total package count") {
    // A -> B -> C and B -> D -> E: longest path is A -> B -> D -> E = 4 layers
    // 5 packages but depth = 4
    val sources = List(
      "ALayer.scala" -> """package A
@layers.dependsOnPackages("B")
object layer
""",
      "BLayer.scala" -> """package B
@layers.dependsOnPackages("C", "D")
object layer
""",
      "CLayer.scala" -> """package C
@layers.dependsOnPackages()
object layer
""",
      "DLayer.scala" -> """package D
@layers.dependsOnPackages("E")
object layer
""",
      "ELayer.scala" -> """package E
@layers.dependsOnPackages()
object layer
""",
      "AUser.scala" -> """package A
class AUser(x: B.BUser)
""",
      "BUser.scala" -> """package B
class BUser(x: C.CUser, y: D.DUser)
""",
      "CUser.scala" -> """package C
class CUser
""",
      "DUser.scala" -> """package D
class DUser(x: E.EUser)
""",
      "EUser.scala" -> """package E
class EUser
"""
    )
    val resultFail = CompilerPluginTestHelper.compile(sources, pluginJarPath, "maxLayers=3")
    assert(resultFail.hasErrors, s"Expected compilation to fail with maxLayers=3 and depth 4. Output: ${resultFail.errorMessages}")
    assert(
      resultFail.errorMessages.contains("4 layers"),
      s"Expected '4 layers' in error. Output: ${resultFail.errorMessages}"
    )

    val resultPass = CompilerPluginTestHelper.compile(sources, pluginJarPath, "maxLayers=4")
    assert(!resultPass.hasErrors, s"Expected compilation to succeed with maxLayers=4. Errors: ${resultPass.errorMessages}")
  }
