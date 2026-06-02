package layers

/** Annotations for the `layer` object to declare allowed package dependencies.
  *
  * By default, no cross-package dependencies are allowed. Add these annotations to the `layer` object
  * in each package to declare which packages that package may depend on. Stdlib packages (scala.*, java.*)
  * are always allowed and need not be listed.
  *
  * The object must be named `layer`. You can declare allowed packages by string:
  * {{{
  *   package application
  *
  *   @dependsOnPackages("domain")
  *   object layer
  * }}}
  *
  * Or by referring to another package `layer` object:
  *
  * {{{
  *   @dependsOnLayers(domain.layer, application.layer)
  *   object layer
  * }}}
  */
class dependsOnPackages(packages: String*) extends scala.annotation.StaticAnnotation

class dependsOnLayers(layers: Any*) extends scala.annotation.StaticAnnotation
