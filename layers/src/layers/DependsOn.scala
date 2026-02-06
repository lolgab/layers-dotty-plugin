package layers

/** Annotation for the `layer` object to declare allowed package dependencies.
  *
  * By default, no cross-package dependencies are allowed. Add this annotation to the `layer` object
  * in each package to declare which packages that package may depend on. Stdlib packages (scala.*, java.*)
  * are always allowed and need not be listed.
  *
  * The object must be named `layer`. Example:
  * {{{
  *   package application
  *
  *   @dependsOn("domain")
  *   object layer
  * }}}
  *
  * This allows the `application` package to depend on `domain` and any subpackage (e.g. `domain.user`).
  * Multiple packages can be listed:
  *
  * {{{
  *   @dependsOn("domain", "application")
  *   object layer
  * }}}
  */
class dependsOn(packages: String*) extends scala.annotation.StaticAnnotation
