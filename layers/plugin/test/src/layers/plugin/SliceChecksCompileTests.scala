package layers.plugin

import java.nio.file.Files

import munit.FunSuite

/** The dependency-based checks through the compiler, with baseline. */
class SliceChecksCompileTests extends FunSuite:

  private def pluginJarPath: String =
    Option(System.getProperty("layers.plugin.jar")).getOrElse(sys.error("layers.plugin.jar system property not set"))

  private def sources(n: Int) =
    val targets = (1 to n).toList.flatMap { i =>
      List(s"T$i.scala" -> s"package t$i\nobject layer\nobject T$i { def v: Int = $i }\n")
    }
    val hub = "Hub.scala" ->
      s"""package hub
@layers.dependsOnPackages(${(1 to n).map(i => "\"t" + i + "\"").mkString(", ")})
object layer
object Hub { def all: Int = ${(1 to n).map(i => s"t$i.T$i.v").mkString(" + ")} }
"""
    hub :: targets

  private def compile(n: Int, opts: String*) =
    CompilerPluginTestHelper.compile(sources(n), pluginJarPath, "design" :: opts.toList)

  test("a slice over its fan-out limit fails with the dependencies to cut; baseline accepts it and locks improvements") {
    val r = compile(4, "maxSliceFanOut=2")
    assert(r.hasErrors && r.errorMessages.contains("Slice `hub` depends on 4 other slices (limit 2)"), r.errorMessages)
    assert(r.errorMessages.contains("hub -> t1"), r.errorMessages)

    val dir = Files.createTempDirectory("layers-slice-baseline-")
    val file = dir.resolve("baseline.txt")
    val base = s"baseline=$file"
    assert(!compile(4, "maxSliceFanOut=2", base, "updateBaseline").hasErrors)
    assert(Files.readString(file).contains("slice-fanout hub = 4"), Files.readString(file))
    assert(!compile(4, "maxSliceFanOut=2", base).hasErrors)
    // a new dependency is worse than the baseline
    assert(compile(5, "maxSliceFanOut=2", base).hasErrors)
    // fixing it without updating the baseline is reported until the improvement is locked in
    val better = compile(3, "maxSliceFanOut=2", base)
    assert(better.hasErrors && better.errorMessages.contains("out of date"), better.errorMessages)
  }

  test("under the limit nothing is reported") {
    val r = compile(4)
    assert(!r.hasErrors, r.errorMessages)
  }
