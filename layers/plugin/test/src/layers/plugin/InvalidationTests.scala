package layers.plugin

import munit.FunSuite

import java.nio.file
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

/** Tests that Zinc incremental compilation invalidates dependent files when the layer object changes.
  *
  * IGNORED: Zinc incremental invalidation does not work with the current plugin/scala-cli setup.
  * The test would require the plugin to register a dependency so that when layer.scala changes,
  * Service gets recompiled. This has not been achieved.
  *
  * Uses scala-cli to run compilation. The test:
  * 1. Creates a temp project with domain (User, layer) and application (Service, layer with @dependsOn)
  * 2. Compiles once
  * 3. Modifies application/layer.scala
  * 4. Compiles again
  * 5. Asserts Service.class was recompiled (mtime changed)
  */
class InvalidationTests extends FunSuite:

  private def pluginJarPath: String =
    Option(System.getProperty("layers.plugin.jar")).getOrElse(
      sys.error("layers.plugin.jar system property not set. Tests must run with forkArgs from build.")
    )

  test("changing layer object invalidates dependent classes (Zinc incremental)".ignore) {
    // Requires: run `mill __.publishLocal` before this test (scala-cli -P needs plugin in ivy2Local)
    val tempDir = Files.createTempDirectory("layers-invalidation-test-")
    try
      val srcDir = tempDir.resolve("src")
      val domainDir = srcDir.resolve("domain")
      val applicationDir = srcDir.resolve("application")
      Files.createDirectories(domainDir)
      Files.createDirectories(applicationDir)

      // domain/User.scala
      Files.writeString(
        domainDir.resolve("User.scala"),
        """package domain
          |case class User(id: String)
          |""".stripMargin
      )
      // domain/layer.scala
      Files.writeString(
        domainDir.resolve("layer.scala"),
        """package domain
          |object layer
          |""".stripMargin
      )
      // application/layer.scala
      Files.writeString(
        applicationDir.resolve("layer.scala"),
        """package application
          |@layers.dependsOn("domain")
          |object layer
          |""".stripMargin
      )
      // application/Service.scala - depends on domain
      Files.writeString(
        applicationDir.resolve("Service.scala"),
        """package application
          |class Service(u: domain.User)
          |""".stripMargin
      )

      def runCompile: Int =
        val proc = new ProcessBuilder(
          "scala-cli",
          "compile",
          ".",
          "-P", "com.github.lolgab::layers-plugin:0.0.0-0-no-vcs",
          "-r", "ivy2Local",
          "--dep", "com.github.lolgab::layers:0.0.0-0-no-vcs"
        )
          .directory(tempDir.toFile)
          .redirectErrorStream(true)
          .start()
        proc.waitFor()

      val exit1 = runCompile
      assert(exit1 == 0, s"First compile failed")

      val serviceClassOpt = findServiceClass(tempDir)
      assert(serviceClassOpt.isDefined, s"Service.class not found under ${tempDir}")
      val serviceClass = serviceClassOpt.get
      val mtimeBefore = Files.getLastModifiedTime(serviceClass).toMillis

      // Modify layer.scala to trigger invalidation
      Files.writeString(
        applicationDir.resolve("layer.scala"),
        """package application
          |// modified for invalidation test
          |@layers.dependsOn("domain")
          |object layer
          |""".stripMargin
      )

      val exit2 = runCompile
      assert(exit2 == 0, s"Second compile failed")

      val mtimeAfter = Files.getLastModifiedTime(serviceClass).toMillis
      assert(
        mtimeAfter > mtimeBefore,
        s"Service.class was not recompiled when layer changed. " +
          s"mtimeBefore=$mtimeBefore mtimeAfter=$mtimeAfter. " +
          s"Zinc did not invalidate Service - the synthetic layer ref may not be registered."
      )
    finally
      deleteRecursively(tempDir)
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
