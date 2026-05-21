package layers.plugin

import munit.FunSuite

import java.net.URLClassLoader
import java.nio.file.Files

class JsNativeTests extends FunSuite:

  private def pluginJarPath: String =
    Option(System.getProperty("layers.plugin.jar")).getOrElse(
      sys.error("layers.plugin.jar system property not set. Tests must run with forkArgs from build.")
    )

  private val jsNativeAnnotStub =
    "JsNativeAnnotStub.scala" -> """package scala.scalajs
package object js {
  @scala.annotation.meta.field @scala.annotation.meta.getter @scala.annotation.meta.setter
  class native extends scala.annotation.StaticAnnotation
  def native: Nothing = throw new Error("stub")
}
"""

  private val jsObjectStub =
    "JsObjectStub.scala" -> """package scala.scalajs.js
class Object extends AnyRef
"""

  private def jsStubs = List(jsNativeAnnotStub, jsObjectStub)

  private def loadClasses(outputDir: java.nio.file.Path): List[Class[?]] =
    val cl = URLClassLoader(Array(outputDir.toUri.toURL), this.getClass.getClassLoader)
    val paths = scala.jdk.CollectionConverters.IteratorHasAsScala(
      Files.walk(outputDir).iterator()
    ).asScala.filter(_.toString.endsWith(".class"))
    paths.map { path =>
      val rel = outputDir.relativize(path).toString.replace(".class", "").replace('/', '.')
      Class.forName(rel, false, cl)
    }.toList

  test("plugin skips @js.native classes (no private synthetic members)") {
    val sources = jsStubs ++ List(
      "DomainLayer.scala" -> """package domain
object layer
""",
      "DomainNative.scala" -> """package domain
import scala.scalajs.js

@js.native
class NativeApi
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(!result.hasErrors, s"Expected compilation to succeed. Errors: ${result.errorMessages}")
  }

  test("plugin skips @js.native traits") {
    val sources = jsStubs ++ List(
      "DomainLayer.scala" -> """package domain
object layer
""",
      "DomainNativeTrait.scala" -> """package domain
import scala.scalajs.js

@js.native
trait NativeApi
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(!result.hasErrors, s"Expected compilation to succeed. Errors: ${result.errorMessages}")
  }

  test("plugin skips @js.native trait extending js.Object with js.native method") {
    val outputDir = Files.createTempDirectory("layers-plugin-jsnative-test-")
    try
      val sources = jsStubs ++ List(
        "DomainLayer.scala" -> """package domain
object layer
""",
        "DomainFoo.scala" -> """package domain
import scala.scalajs.js

@js.native
trait Foo extends js.Object:
  def foo(): Unit = js.native
"""
      )
      val result = CompilerPluginTestHelper.compile(sources, pluginJarPath, outputDir = Some(outputDir))
      assert(!result.hasErrors, s"Expected compilation to succeed. Errors: ${result.errorMessages}")

      val domainClasses = loadClasses(outputDir).filter(_.getName.startsWith("domain."))
      val memberLayerRefs = domainClasses.flatMap { cls =>
        val fields = cls.getDeclaredFields.toList.map(_.getName).filter(_.contains("_layerRef"))
        val methods = cls.getDeclaredMethods.toList.map(_.getName).filter(_.contains("_layerRef"))
        (fields ++ methods).map(name => s"${cls.getName}.$name")
      }
      assertEquals(
        memberLayerRefs,
        Nil,
        s"Native JS types may not have private members: plugin must not add _layerRef to @js.native trait Foo"
      )
    finally
      Files.walk(outputDir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))
      Files.deleteIfExists(outputDir)
  }

  test("plugin skips @js.native object extending js.Object with native members") {
    val jsExtraStubs = List(
      "JsAnnotStubs.scala" -> """package scala.scalajs.js
type UndefOr[+A] = A | Null
""",
      "JsAnnotationStubs.scala" -> """package scala.scalajs.js.annotation
class JSGlobalScope extends scala.annotation.StaticAnnotation
class JSName(name: String) extends scala.annotation.StaticAnnotation
""",
      "LeafletStub.scala" -> """package domain
import scala.scalajs.js

@js.native
trait LeafletGlobal extends js.Object
"""
    )
    val outputDir = Files.createTempDirectory("layers-plugin-jsnative-test-")
    try
      val sources = jsStubs ++ jsExtraStubs ++ List(
        "DomainLayer.scala" -> """package domain
object layer
""",
        "JsGlobalScope.scala" -> """package domain
import scala.scalajs.js
import scala.scalajs.js.annotation._

@js.native
@JSGlobalScope
object JsGlobalScope extends js.Object:
  val L: js.UndefOr[LeafletGlobal] = js.native
  @JSName("Chart")
  val Chart: js.UndefOr[js.Object] = js.native
"""
      )
      val result = CompilerPluginTestHelper.compile(sources, pluginJarPath, outputDir = Some(outputDir))
      assert(!result.hasErrors, s"Expected compilation to succeed. Errors: ${result.errorMessages}")

      val domainClasses = loadClasses(outputDir).filter(_.getName.startsWith("domain."))
      val memberLayerRefs = domainClasses.flatMap { cls =>
        val fields = cls.getDeclaredFields.toList.map(_.getName).filter(_.contains("_layerRef"))
        val methods = cls.getDeclaredMethods.toList.map(_.getName).filter(_.contains("_layerRef"))
        (fields ++ methods).map(name => s"${cls.getName}.$name")
      }
      assertEquals(
        memberLayerRefs,
        Nil,
        "Native JS types may not have private members: plugin must not add _layerRef to @js.native object JsGlobalScope"
      )
    finally
      Files.walk(outputDir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))
      Files.deleteIfExists(outputDir)
  }

  test("plugin skips classes extending @js.native traits") {
    val sources = jsStubs ++ List(
      "DomainLayer.scala" -> """package domain
object layer
""",
      "DomainNativeImpl.scala" -> """package domain
import scala.scalajs.js

@js.native
trait NativeApi

class NativeImpl extends NativeApi
"""
    )
    val result = CompilerPluginTestHelper.compile(sources, pluginJarPath)
    assert(!result.hasErrors, s"Expected compilation to succeed. Errors: ${result.errorMessages}")
  }
