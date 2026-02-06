package layers.plugin

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter

import java.nio.file.Files

/** Helper for in-process compiler plugin testing, inspired by munit-compiler-toolkit.
  *
  * Compiles Scala source strings with the layers plugin in-process instead of launching mill.
  */
object CompilerPluginTestHelper:

  case class CompilationResult(hasErrors: Boolean, errorMessages: String, warningMessages: String = "")

  /** Compile the given sources with the layers plugin.
    *
    * @param sources
    *   List of (filename, content) for each compilation unit. Use distinct filenames for cycle
    *   detection (e.g. "User.scala", "Bar.scala").
    * @param pluginJarPath
    *   Path to the assembled plugin jar
    * @param layersOptions
    *   Plugin options (optional, annotation-based config uses no options).
    */
  def compile(
      sources: List[(String, String)],
      pluginJarPath: String,
      layersOptions: String | List[String] = Nil
  ): CompilationResult =
    val opts = layersOptions match
      case s: String     => List(s)
      case l: List[?]    => l.asInstanceOf[List[String]]
    compileWithOptions(sources, pluginJarPath, opts)

  private def compileWithOptions(
      sources: List[(String, String)],
      pluginJarPath: String,
      layersOptions: List[String]
  ): CompilationResult =
    val tempDir = Files.createTempDirectory("layers-plugin-test-")
    val filePaths = sources.map { case (name, content) =>
      val path = tempDir.resolve(name)
      Files.writeString(path, content)
      path.toAbsolutePath.toString
    }
    try
      val classpath = Thread.currentThread.getContextClassLoader match
        case cl: java.net.URLClassLoader =>
          cl.getURLs.iterator.map(_.getPath).mkString(java.io.File.pathSeparator)
        case _ =>
          System.getProperty("java.class.path")
      val pluginArgs = layersOptions.map(opt => "-P:layers:" + opt)
      val args = Array(
        "-classpath",
        classpath,
        "-Xplugin:" + pluginJarPath,
        "-deprecation"
      ) ++ pluginArgs ++ filePaths
      val reporter = new StoreReporter(null)
      val driver = new Driver
      driver.process(args, reporter)
      val errorMessages = reporter.allErrors.map(_.message.toString).mkString("\n")
      val warningMessages = reporter.allWarnings.map(_.message.toString).mkString("\n")
      CompilationResult(reporter.hasErrors, errorMessages, warningMessages)
    finally
      filePaths.foreach(p => Files.deleteIfExists(java.nio.file.Paths.get(p)))
      Files.deleteIfExists(tempDir)
