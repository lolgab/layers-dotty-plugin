package layers.plugin

import layers.plugin.CompilerPluginTestHelper.CompilationResult

/** Compiles small projects with the design checks on, for the architecture check tests. */
object ArchTestSupport:

  def pluginJar: String =
    Option(System.getProperty("layers.plugin.jar")).getOrElse(
      sys.error("layers.plugin.jar system property not set. Tests must run with forkArgs from build.")
    )

  def design(sources: List[(String, String)], options: String*): CompilationResult =
    CompilerPluginTestHelper.compile(sources, pluginJar, "design" :: options.toList)
