package layers.plugin

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.*

object SymbolNames:

  /** Dotted package of a symbol, or "root" for the empty package. */
  def packageOf(sym: Symbol)(using Context): String =
    def loop(s: Symbol): String =
      if s == null || !s.exists || s.isRoot || s == defn.RootPackage then ""
      else if s.is(Flags.Package) then
        val rest = loop(s.owner)
        if rest.isEmpty then s.name.toString else s"$rest.${s.name}"
      else loop(s.owner)
    val pkg = loop(sym)
    if pkg.isEmpty then "root" else pkg

  def isStdLib(pkg: String): Boolean =
    pkg.startsWith("scala.") || pkg.startsWith("java.") || pkg == "scala" || pkg == "java"

  /** Name of the top-level class or object containing a symbol, the unit that moves between packages. */
  def topLevelName(sym: Symbol)(using Context): String =
    val top = sym.topLevelClass
    if top.exists then top.fullName.toString.stripSuffix("$") else ""
