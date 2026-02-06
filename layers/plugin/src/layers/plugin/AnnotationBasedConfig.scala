package layers.plugin

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.*
/** Configuration built from @dependsOn annotations on the `layer` object.
  *
  * By default, no cross-package dependencies are allowed. A package may only depend on another
  * package if its `layer` object has @dependsOn listing that package. Stdlib (scala.*, java.*)
  * is always allowed.
  */
class AnnotationBasedConfig(using Context):

  private def dependsOnAnnotClass(using Context): Symbol =
    getClassIfDefined("layers.dependsOn")

  /** Returns the set of package prefixes that the given package is allowed to depend on.
    * Stdlib (scala, java) is always included. Returns empty set if the package has no
    * `layer` object or no @dependsOn annotation.
    */
  def allowedDependenciesFor(pkg: String): Set[String] =
    if pkg.isEmpty || pkg == "root" then Set.empty
    else
      val fromAnnotation = layerObjectDependsOn(pkg)
      fromAnnotation + "scala" + "java"

  /** Returns true if ownerPackage is allowed to depend on refPackage. */
  def isAllowed(ownerPackage: String, refPackage: String): Boolean =
    if refPackage.isEmpty || refPackage == "root" then true
    else if isStdLib(refPackage) then true
    else if ownerPackage == refPackage then true
    else if refPackage.startsWith(ownerPackage + ".") then true // same package or subpackage
    else
      val allowed = allowedDependenciesFor(ownerPackage)
      allowed.exists { prefix =>
        refPackage == prefix || refPackage.startsWith(prefix + ".")
      }

  private def isStdLib(pkg: String): Boolean =
    pkg == "scala" || pkg.startsWith("scala.") || pkg == "java" || pkg.startsWith("java.")

  private def layerObjectDependsOn(pkg: String)(using Context): Set[String] =
    try
      val pkgSym = requiredPackage(pkg)
      val pkgClass = pkgSym.moduleClass
      val layerObjectOpt = pkgClass.info.decls.iterator
        .find(s => s.name.toString == "layer")
      layerObjectOpt match
        case None => Set.empty
        case Some(layerObjSym) =>
          val cls = if layerObjSym.is(Flags.ModuleVal) then layerObjSym.moduleClass else layerObjSym.asClass
          if !cls.exists then Set.empty
          else
            val annotClass = dependsOnAnnotClass
            if !annotClass.exists then Set.empty
            else
              val annot = cls.getAnnotation(annotClass)
              annot.fold(Set.empty[String])(extractDependsOnPackages)
    catch case _: Exception => Set.empty

  private def extractDependsOnPackages(annot: dotty.tools.dotc.core.Annotations.Annotation)(using Context): Set[String] =
    import dotty.tools.dotc.ast.tpd.{SeqLiteral, Literal, Typed}
    annot.arguments.flatMap {
      case SeqLiteral(elems, _) =>
        elems.flatMap {
          case Literal(const) =>
            const.value match
              case s: String => Some(s)
              case _        => None
          case _ => None
        }
      case Typed(SeqLiteral(elems, _), _) =>
        elems.flatMap {
          case Literal(const) =>
            const.value match
              case s: String => Some(s)
              case _        => None
          case _ => None
        }
      case Literal(const) =>
        const.value match
          case s: String => Some(s)
          case _         => None
      case _ => None
    }.toSet
