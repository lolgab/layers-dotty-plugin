package layers.plugin

import java.io.File
import java.net.URLClassLoader
import java.util.Optional
import java.util.concurrent.atomic.AtomicReference

import scala.jdk.CollectionConverters.*

import sbt.internal.inc.*
import sbt.internal.inc.classpath.ClasspathUtil
import sbt.internal.inc.consistent.ConsistentFileAnalysisStore
import xsbti.compile.*
import xsbti.compile.analysis.ReadWriteMappers
import xsbti.VirtualFile

/** Runs incremental Scala compilation using vanilla Zinc API (no Mill, sbt, scala-cli).
  *
  * Based on Mill's ZincWorker; uses sbt.internal.inc.IncrementalCompilerImpl directly.
  */
object ZincIncrementalRunner:

  case class CompilationRoundResult(exitCode: Int, output: String)

  /** Run multiple compilation rounds with Zinc incremental compilation.
    *
    * @param rounds
    *   Each round is a full snapshot of sources: (path relative to src/, content). Between rounds,
    *   Zinc's analysis is preserved for incremental compilation. To force a clean compile, pass
    *   clearCache = true for that round (not implemented – always incremental after round 1).
    * @param pluginJarPath
    *   Path to the layers plugin jar
    * @param layersJarPath
    *   Path to the layers library jar
    * @param scalaVersion
    *   Scala version (e.g. "3.8.1")
    */
  def runCompilationRounds(
      rounds: List[List[(String, String)]],
      pluginJarPath: String,
      layersJarPath: String,
      scalaVersion: String = "3.8.1"
  ): List[CompilationRoundResult] =
    val tempDir = java.nio.file.Files.createTempDirectory("layers-zinc-test-").toAbsolutePath
    try
      val srcDir = tempDir.resolve("src")
      val workDir = tempDir.resolve("work")
      java.nio.file.Files.createDirectories(srcDir)
      java.nio.file.Files.createDirectories(workDir)

      val outputRef = new AtomicReference[StringBuilder](new StringBuilder)
      val inc = new IncrementalCompilerImpl()
      val (compilers, bridgePath) = setupCompilers(scalaVersion, pluginJarPath)
      val store = fileAnalysisStore(workDir.resolve("zinc").toFile)
      val converter = MappedFileConverter.empty

      rounds.zipWithIndex.map { case (roundSources, roundIdx) =>
        for (relPath, content) <- roundSources do
          val path = srcDir.resolve(relPath)
          java.nio.file.Files.createDirectories(path.getParent)
          java.nio.file.Files.writeString(path, content)

        val sources = java.nio.file.Files
          .walk(srcDir)
          .iterator()
          .asScala
          .filter(p => java.nio.file.Files.isRegularFile(p) && p.toString.endsWith(".scala"))
          .map(_.toAbsolutePath)
          .toSeq

        val classesDir = workDir.resolve("classes")
        java.nio.file.Files.createDirectories(classesDir)

        val classpath = (Seq(layersJarPath, classesDir.toString).iterator ++
          getClasspathStrings().iterator)
          .map(p => converter.toVirtualFile(java.nio.file.Paths.get(p)))
          .toArray
        val virtualSources = sources
          .map(p => converter.toVirtualFile(p))
          .toArray

        val output = new StringBuilder
        outputRef.set(output)
        def append(s: String): Unit = output.append(s).append("\n")
        val capturingReporter = new CapturingReporter(output)
        val simpleLogger = new sbt.util.Logger {
          override def log(level: sbt.util.Level.Value, msg: => String): Unit =
            if level > sbt.util.Level.Debug then append(msg)
          override def trace(t: => Throwable): Unit = append(t.toString)
          override def success(msg: => String): Unit = append(msg)
        }

        val maxErrors = 100
        val lookup = new PerClasspathEntryLookup:
          override def analysis(classpathEntry: VirtualFile): Optional[CompileAnalysis] =
            Optional.empty()
          override def definesClass(classpathEntry: VirtualFile): DefinesClass =
            if classpathEntry.name.toString != "rt.jar" then Locate.definesClass(classpathEntry)
            else (_: String) => false

        val prevOpt = if roundIdx > 0 then store.get() else Optional.empty[AnalysisContents]()
        val pr = if prevOpt.isPresent then
          val contents = prevOpt.get()
          PreviousResult.of(Optional.of(contents.getAnalysis), Optional.of(contents.getMiniSetup))
        else
          PreviousResult.of(Optional.empty[CompileAnalysis], Optional.empty[MiniSetup])

        val reporter = capturingReporter
        val incOptions = IncOptions.of()
        val setup = inc.setup(
          lookup = lookup,
          skip = false,
          cacheFile = workDir.resolve("zinc").toFile.toPath,
          cache = new FreshCompilerCache,
          incOptions = incOptions,
          reporter = reporter,
          progress = None,
          earlyAnalysisStore = None,
          extra = Array()
        )

        val scalacOptions = Array(
          "-Xplugin:" + pluginJarPath,
          "-deprecation",
          "-color:never"
        )

        val inputs = inc.inputs(
          classpath = classpath,
          sources = virtualSources,
          classesDirectory = classesDir,
          earlyJarPath = None,
          scalacOptions = scalacOptions,
          javacOptions = Array(),
          maxErrors = maxErrors,
          sourcePositionMappers = Array(),
          order = CompileOrder.Mixed,
          compilers = compilers,
          setup = setup,
          pr = pr,
          temporaryClassesDirectory = Optional.empty(),
          converter = converter,
          stampReader = Stamps.timeWrapBinaryStamps(converter)
        )

        val exitCode = try
          val result = inc.compile(in = inputs, logger = simpleLogger)
          store.set(AnalysisContents.create(result.analysis(), result.setup()))
          0
        catch
          case _: CompileFailed => 1

        CompilationRoundResult(exitCode, output.toString())
      }
    finally
      deleteRecursively(tempDir)

  private def setupCompilers(
      scalaVersion: String,
      pluginJarPath: String
  ): (Compilers, java.nio.file.Path) =
    val classpath = getClasspathStrings()
    val compilerPaths = classpath.filter { p =>
      p.contains("scala3-compiler") || p.contains("scala-library") ||
      p.contains("scala3-library") || p.contains("scala-reflect")
    }
    val bridgePath = classpath.find(_.contains("scala3-sbt-bridge")).getOrElse(
      sys.error("scala3-sbt-bridge not found on classpath. Add org.scala-lang:scala3-sbt-bridge")
    )
    val libraryJar = if scalaVersion.startsWith("3.8") then
      classpath.find(_.contains("scala-library")).getOrElse(
        sys.error("scala-library not found for Scala 3.8+")
      )
    else
      classpath.find(p => p.contains("scala-library") && p.contains("2.13")).getOrElse(
        classpath.find(_.contains("scala-library")).getOrElse(
          sys.error("scala-library not found")
        )
      )

    val compilerJars = compilerPaths.map(p => new File(p)).toArray
    val classLoader = new URLClassLoader(
      (compilerPaths :+ pluginJarPath).distinct.map(p => new java.io.File(p).toURI.toURL).toArray,
      Thread.currentThread().getContextClassLoader
    )
    val scalaInstance = new sbt.internal.inc.ScalaInstance(
      version = scalaVersion,
      loader = classLoader,
      loaderCompilerOnly = classLoader,
      loaderLibraryOnly = ClasspathUtil.rootLoader,
      libraryJars = Array(new File(libraryJar)),
      compilerJars = compilerJars,
      allJars = (compilerJars :+ new File(pluginJarPath)).distinct,
      explicitActual = None
    )
    val scalaCompiler = ZincUtil.scalaCompiler(scalaInstance, new File(bridgePath))
    val javaTools = javac.JavaTools(
      javac.JavaCompiler.local.getOrElse(javac.JavaCompiler.fork()),
      javac.Javadoc.local.getOrElse(javac.Javadoc.fork())
    )
    val compilers = (new IncrementalCompilerImpl()).compilers(javaTools, scalaCompiler)
    (compilers, java.nio.file.Paths.get(bridgePath))

  private def getClasspathStrings(): Seq[String] =
    Thread.currentThread().getContextClassLoader match
      case cl: URLClassLoader =>
        cl.getURLs.iterator.map(_.getPath).filter(_.endsWith(".jar")).toSeq
      case _ =>
        sys.props.getOrElse("java.class.path", "").split(File.pathSeparator).toSeq

  private def fileAnalysisStore(path: File): AnalysisStore =
    ConsistentFileAnalysisStore.binary(
      file = path,
      mappers = ReadWriteMappers.getEmptyMappers,
      reproducible = true,
      parallelism = math.min(Runtime.getRuntime.availableProcessors(), 8)
    )

  /** Minimal Reporter that captures problems to a StringBuilder. */
  private class CapturingReporter(output: StringBuilder) extends xsbti.Reporter:
    private val problemsBuffer = scala.collection.mutable.ArrayBuffer.empty[xsbti.Problem]

    override def reset(): Unit = problemsBuffer.clear()
    override def hasErrors(): Boolean = problemsBuffer.exists(_.severity == xsbti.Severity.Error)
    override def hasWarnings(): Boolean = problemsBuffer.exists(_.severity == xsbti.Severity.Warn)
    override def printSummary(): Unit =
      problemsBuffer.foreach { p =>
        output.append(p.message()).append("\n")
      }
    override def problems(): Array[xsbti.Problem] = problemsBuffer.toArray
    override def log(problem: xsbti.Problem): Unit =
      problemsBuffer += problem
      output.append(problem.message()).append("\n")
    override def comment(pos: xsbti.Position, msg: String): Unit = output.append(msg).append("\n")

  private def deleteRecursively(path: java.nio.file.Path): Unit =
    if java.nio.file.Files.exists(path) then
      if java.nio.file.Files.isDirectory(path) then
        java.nio.file.Files.list(path).forEach(deleteRecursively)
      java.nio.file.Files.delete(path)
