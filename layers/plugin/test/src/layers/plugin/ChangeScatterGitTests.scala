package layers.plugin

import java.nio.file.{Files, Path}

import munit.FunSuite

/** The history check against a real git repository. */
class ChangeScatterGitTests extends FunSuite:

  private def pluginJarPath: String =
    Option(System.getProperty("layers.plugin.jar")).getOrElse(sys.error("layers.plugin.jar system property not set"))

  private def sh(dir: Path, args: String*): Unit =
    val p = new ProcessBuilder((List("git", "-C", dir.toString, "-c", "user.name=t", "-c", "user.email=t@t") ++ args)*)
      .redirectErrorStream(true).start()
    p.getInputStream.readAllBytes()
    assertEquals(p.waitFor(), 0)

  private val files = List(
    "a/A.scala" -> "package a\nobject A\n",
    "b/B.scala" -> "package b\nobject B\n",
    "c/C.scala" -> "package c\nobject C\n"
  )

  /** A repository where a and b change together in 10 commits and c alone in 12. */
  private def repo(): Path =
    val dir = Files.createTempDirectory("layers-scatter-")
    files.foreach { case (n, c) =>
      Files.createDirectories(dir.resolve(n).getParent)
      Files.writeString(dir.resolve(n), c)
    }
    sh(dir, "init", "-q")
    sh(dir, "add", ".")
    sh(dir, "commit", "-q", "-m", "start")
    def touch(n: String, i: Int) = Files.writeString(dir.resolve(n), files.toMap.apply(n) + s"// $i\n")
    for i <- 1 to 10 do
      touch("a/A.scala", i)
      touch("b/B.scala", i)
      sh(dir, "commit", "-q", "-am", s"feature $i")
    for i <- 1 to 12 do
      touch("c/C.scala", i)
      sh(dir, "commit", "-q", "-am", s"other $i")
    // leave the working tree as the compile will write it
    Files.writeString(dir.resolve("a/A.scala"), files.toMap.apply("a/A.scala") + "// 10\n")
    Files.writeString(dir.resolve("b/B.scala"), files.toMap.apply("b/B.scala") + "// 10\n")
    Files.writeString(dir.resolve("c/C.scala"), files.toMap.apply("c/C.scala") + "// 12\n")
    dir

  private def compile(dir: Path, opts: String*) =
    val withTail = files.map { case (n, c) =>
      n -> (c + (if n == "a/A.scala" || n == "b/B.scala" then "// 10\n" else "// 12\n"))
    }
    CompilerPluginTestHelper.compile(withTail, pluginJarPath, "design" :: opts.toList, sourceDir = Some(dir))

  test("a repository where two packages change together fails with the commit count, and is silent without history=") {
    val dir = repo()
    val on = compile(dir, "history=500 commits")
    assert(on.hasErrors, on.errorMessages)
    assert(on.errorMessages.contains("Packages `a` and `b` change together"), on.errorMessages)
    assert(on.errorMessages.contains("commits used of"), on.errorMessages)
    val off = compile(dir)
    assert(!off.hasErrors, off.errorMessages)
  }

  test("baseline accepts the finding, and a run without history leaves its entry alone") {
    val dir = repo()
    val file = dir.resolve("baseline.txt")
    val base = s"baseline=$file"
    val init = compile(dir, "history=500 commits", base, "updateBaseline")
    assert(!init.hasErrors, init.errorMessages)
    assert(Files.readString(file).contains("co-change a <-> b"), Files.readString(file))
    val again = compile(dir, "history=500 commits", base)
    assert(!again.hasErrors, again.errorMessages)
    // history not configured: the entry is not evaluated, so it is neither stale nor fixed
    val without = compile(dir, base)
    assert(!without.hasErrors, without.errorMessages)
    assert(Files.readString(file).contains("co-change a <-> b"))
  }

  test("outside a git repository the check is skipped with a warning and never fails") {
    val dir = Files.createTempDirectory("layers-nogit-")
    val r = compile(dir, "history=500 commits")
    assert(!r.hasErrors, r.errorMessages)
    assert(r.warningMessages.contains("co-change: skipped"), r.warningMessages)
  }

  test("a shallow clone is skipped with a warning") {
    val origin = repo()
    val dir = Files.createTempDirectory("layers-shallow-")
    val p = new ProcessBuilder("git", "clone", "-q", "--depth", "3", "file://" + origin.toString, dir.resolve("c").toString).redirectErrorStream(true).start()
    p.getInputStream.readAllBytes()
    assertEquals(p.waitFor(), 0)
    val r = compile(dir.resolve("c"), "history=500 commits")
    assert(!r.hasErrors, r.errorMessages)
    assert(r.warningMessages.contains("shallow"), r.warningMessages)
  }

  test("since=<rev> enforces a co-change finding only when its files changed") {
    val dir = repo()
    val untouched = compile(dir, "history=500 commits", "since=HEAD")
    assert(!untouched.hasErrors, untouched.errorMessages)
    assert(untouched.warningMessages.contains("existing findings"), untouched.warningMessages)
  }
