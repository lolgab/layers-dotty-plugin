package layers.plugin

import munit.FunSuite

import java.nio.file
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/** Tests for Zinc incremental compilation behavior.
  *
  * The plugin validates layer dependencies at compile time. Zinc recompilation when @dependsOn
  * changes depends on the build tool's dependency tracking.
  */
class InvalidationTests extends FunSuite:

  type CompilationRoundResult = ZincIncrementalRunner.CompilationRoundResult

  /** Run multiple compilation rounds with vanilla Zinc incremental compilation.
    * Each round is a full snapshot of sources: (path relative to src/, content).
    * Between rounds, Zinc's analysis is preserved for incremental compilation.
    */
  def runCompilationRounds(rounds: List[List[(String, String)]]): List[CompilationRoundResult] =
    ZincIncrementalRunner.runCompilationRounds(
      rounds = rounds,
      pluginJarPath = pluginJarPath,
      layersJarPath = layersJarPath
    )

  private def pluginJarPath: String =
    Option(System.getProperty("layers.plugin.jar")).getOrElse(
      sys.error("layers.plugin.jar system property not set. Tests must run with forkArgs from build.")
    )

  private def layersJarPath: String =
    Option(System.getProperty("layers.jar")).getOrElse(
      sys.error("layers.jar system property not set. Tests must run with forkArgs from build.")
    )

  test("plugin compiles successfully with application layer") {
    val sources = List(
      "ApplicationLayer.scala" -> """package application
@layers.dependsOn("domain")
object layer
""",
      "ApplicationService.scala" -> """package application
class Service(u: domain.User)
""",
      "DomainUser.scala" -> """package domain
case class User(id: String)
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(!result.hasErrors, s"Expected compilation to succeed. Errors: ${result.errorMessages}")
  }

  test("incremental (Zinc): changing @dependsOn - no false positives for remaining layers") {
    val rounds = List(
      List(
        "domain/User.scala" -> """package domain
case class User(id: String)
""",
        "domain/layer.scala" -> """package domain
object layer
""",
        "application/layer.scala" -> """package application
@layers.dependsOn("domain")
object layer
""",
        "application/Service.scala" -> """package application
class Service(u: domain.User)
""",
        "infrastructure/layer.scala" -> """package infrastructure
@layers.dependsOn("domain", "application")
object layer
""",
        "infrastructure/Repo.scala" -> """package infrastructure
class Repo(u: domain.User)
""",
        "presentation/layer.scala" -> """package presentation
@layers.dependsOn("infrastructure", "application")
object layer
""",
        "presentation/Cli.scala" -> """package presentation
class Cli(s: application.Service, r: infrastructure.Repo)
""",
        "main/layer.scala" -> """package main
@layers.dependsOn("domain", "application", "infrastructure", "presentation")
object layer
""",
        "main/Main.scala" -> """package main
object Main:
  def main(args: Array[String]): Unit =
    val repo = infrastructure.Repo(domain.User("x"))
    val service = application.Service(domain.User("y"))
    val cli = presentation.Cli(service, repo)
"""
      ),
      List(
        "main/layer.scala" -> """package main
@layers.dependsOn("domain", "infrastructure", "presentation")
object layer
"""
      )
    )
    val results = runCompilationRounds(rounds)
    assert(results.length == 2)
    assert(results(0).exitCode == 0, s"Round 1 failed: ${results(0).output}")
    val round2 = results(1)
    assert(
      round2.exitCode != 0,
      s"Round 2 should fail (main uses application, removed from @dependsOn). exitCode=${round2.exitCode} output=${round2.output.take(2000)}"
    )
    assert(
      !round2.output.contains("cannot depend on infrastructure"),
      s"Should NOT get false error for infrastructure (main still depends on it). Output: ${round2.output}"
    )
    assert(
      !round2.output.contains("cannot depend on presentation"),
      s"Should NOT get false error for presentation (main still depends on it). Output: ${round2.output}"
    )
    assert(
      round2.output.contains("cannot depend on application"),
      s"Should get error for application (removed from @dependsOn). Output: ${round2.output}"
    )
  }

  private def findServiceClass(root: file.Path): Option[file.Path] =
    val candidates = Files.walk(root).iterator().asScala.filter { p =>
      p.getFileName != null && p.getFileName.toString == "Service.class" &&
        p.toString.contains("application")
    }.toSeq
    // Prefer classes/main (stable); bloop-internal-classes paths change between runs
    candidates.find(_.toString.contains("classes/main")).orElse(candidates.headOption)

  private def deleteRecursively(path: file.Path): Unit =
    if Files.exists(path) then
      if Files.isDirectory(path) then
        Files.list(path).forEach(deleteRecursively)
      Files.delete(path)
