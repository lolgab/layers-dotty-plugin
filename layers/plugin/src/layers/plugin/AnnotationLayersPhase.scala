package layers.plugin

import scala.collection.mutable

import java.security.MessageDigest

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Definitions.*
import dotty.tools.dotc.core.Constants.*
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.YCheckPositions

/** Plugin phase that enforces package dependencies via @dependsOn annotations on the `layer` object.
  * By default nothing is allowed; packages must declare their dependencies explicitly.
  * @dependsOn may only be placed on `object layer`; it fails on classes, traits, or objects with other names.
  */
class AnnotationLayersPhase(options: List[String] = Nil)(using Context) extends PluginPhase:
  import tpd.*

  val phaseName = "layers"

  override val runsAfter = Set(YCheckPositions.name)

  private val fileDeps = mutable.Map[String, mutable.Set[String]]()

  /** Package -> allowed package prefixes, from @dependsOn annotations on the `layer` object */
  private val packageToAllowed = mutable.Map[String, Set[String]]()

  /** Package -> hash hex for Zinc invalidation. Hash derived from @dependsOn content. */
  private val packageToHashHex = mutable.Map[String, String]()

  /** Special hash used when a package has no layer object. Ensures Zinc invalidation semantics. */
  private val noLayersHash = "nolayers"

  /** Packages we've seen in the tree traversal (for second-pass package symbol fallback). */
  private val packagesSeenInTree = mutable.Set[String]()

  private val maxLayers: Option[Int] =
    options.collectFirst { case s if s.startsWith("maxLayers=") => s.drop(10).trim.toIntOption }.flatten.filter(_ > 0)

  override def run(using ctx: Context): Unit =
    val units = ctx.run.nn.units
    // Pass 1: collect @dependsOn from trees only. This uses fresh data from the current run.
    // During incremental compilation, package symbol lookup may return stale classpath symbols.
    for unit <- units do
      if !unit.tpdTree.isEmpty then
        validateDependsOnPlacement(unit.tpdTree)
        val sourceContent = if unit.source != null && unit.source.exists then
          Some(new String(unit.source.content()))
        else None
        collectDependsOnFromTree(unit.tpdTree, sourceContent)
        collectDependsOnFromSymbols(unit.tpdTree, sourceContent)
    // Pass 2: for packages we've seen but don't have layer info from trees, fall back to
    // package symbol (classpath). Handles e.g. compiling only Service.scala when layer.scala
    // is not in this run.
    for pkg <- packagesSeenInTree do
      if !packageToAllowed.contains(pkg) then collectDependsOnFromPackageSymbol(pkg)
    maxLayers.foreach { limit =>
      val layerCount = packageToAllowed.size
      if layerCount > limit then
        val pos = units.iterator.flatMap(u => if u.tpdTree.isEmpty then None else Some(u.tpdTree.srcPos)).nextOption().getOrElse(
          throw new IllegalStateException("maxLayers check requires at least one compilation unit with source")
        )
        report.error(
          s"Application ${maxLayersExceededMessage(layerCount, limit)} (e.g. -P:layers:maxLayers=$layerCount).",
          pos
        )
    }
    super.run

  private val dependsOnAnnotName = "dependsOn"

  private def maxLayersExceededMessage(layerCount: Int, limit: Int): String =
    val layersToRemove = layerCount - limit
    val layerOrLayers = if layersToRemove == 1 then "layer" else "layers"
    s"has $layerCount layers but maxLayers is $limit. Remove $layersToRemove $layerOrLayers or increase maxLayers."

  private def hasDependsOnAnnotation(sym: Symbol)(using Context): Boolean =
    if !sym.exists then false
    else if sym.is(Flags.ModuleVal) then
      val cls = sym.moduleClass
      cls.exists && cls.annotations.exists(a => a.symbol.fullName.toString.endsWith(dependsOnAnnotName))
    else if sym.isClass then
      sym.asClass.annotations.exists(a => a.symbol.fullName.toString.endsWith(dependsOnAnnotName))
    else false

  private def isValidDependsOnPlacement(sym: Symbol): Boolean =
    (sym.name.toString == "layer" && sym.is(Flags.ModuleVal)) ||
    (sym.is(Flags.ModuleClass) && sym.sourceModule.exists && sym.sourceModule.name.toString == "layer")

  /** Validates that @dependsOn appears only on `object layer`. Fails on classes, traits, or objects with other names. */
  private def validateDependsOnPlacement(tree: Tree)(using Context): Unit =
    tree match
      case PackageDef(pid, stats) =>
        for stat <- stats do validateStat(stat)
      case _ =>
        ()

  private def validateStat(stat: Tree)(using Context): Unit =
    stat match
      case t: TypeDef =>
        val sym = stat.symbol
        if sym.exists && hasDependsOnAnnotation(sym) && !isValidDependsOnPlacement(sym) then
          val kind = if sym.is(Flags.ModuleClass) then "object" else if sym.is(Flags.Trait) then "trait" else "class"
          val name = if sym.is(Flags.ModuleClass) && sym.sourceModule.exists then sym.sourceModule.name.toString else sym.name.toString
          report.error(
            s"@dependsOn may only be placed on `object layer`. Found on $kind `$name`.",
            stat.srcPos
          )
        // Optionally validate nested classes/objects (skipped: LazyTreeList complexity)
      case v: ValDef if v.symbol.exists && v.symbol.is(Flags.ModuleVal) =>
        val sym = v.symbol
        if hasDependsOnAnnotation(sym) && !isValidDependsOnPlacement(sym) then
          report.error(
            s"@dependsOn may only be placed on `object layer`. Found on object `${sym.name}`.",
            stat.srcPos
          )
      case PackageDef(pid, stats) =>
        for s <- stats do validateStat(s)
      case _ =>
        ()


  private def collectDependsOnFromTree(tree: Tree, sourceContent: Option[String] = None)(using Context): Unit =
    tree match
      case PackageDef(pid, stats) =>
        val pkgName = packageNameFromTree(pid)
        if pkgName.nonEmpty && pkgName != "root" then packagesSeenInTree += pkgName
        for stat <- stats do
          if stat.symbol.exists then
            val sym = stat.symbol
            val isLayerObject = (sym.name.toString == "layer" && sym.is(Flags.ModuleVal)) ||
              (sym.is(Flags.ModuleClass) && sym.sourceModule.exists && sym.sourceModule.name.toString == "layer")
            if isLayerObject then
              val allowed = extractDependsOnFromSymbol(if sym.is(Flags.ModuleVal) then sym else sym.sourceModule)
              packageToAllowed(pkgName) = packageToAllowed.getOrElse(pkgName, Set.empty) ++ allowed
              packageToHashHex(pkgName) = dependsOnHash(allowed, sourceContent)
        for stat <- stats do collectDependsOnFromTree(stat, sourceContent)
      case _ =>

  private def packageNameFromTree(tree: Tree)(using Context): String =
    tree match
      case Ident(name) => name.toString
      case Select(qual, name) => s"${packageNameFromTree(qual)}.${name}"
      case _ => ""

  /** Look up the layer object from the classpath (preferred) or package symbol.
    * During Zinc incremental compilation, only a subset of sources may be in the run (e.g. only
    * Service.scala when layer.scala was compiled in a previous cycle). Fetch from classpath first
    * via requiredModule so we find layers not compiled in this run.
    */
  private def collectDependsOnFromPackageSymbol(pkgName: String)(using Context): Unit =
    if pkgName.isEmpty || pkgName == "root" then return
    val s = resolveLayerSymbol(pkgName)
    val layerObjOpt = if s != null && s.exists then Some(s) else None
    layerObjOpt match
      case None => ()
      case Some(layerObjSym) =>
        val allowed = extractDependsOnFromSymbol(layerObjSym)
        packageToAllowed(pkgName) = packageToAllowed.getOrElse(pkgName, Set.empty) ++ allowed
        if !packageToHashHex.contains(pkgName) then packageToHashHex(pkgName) = dependsOnHash(allowed)

  /** Fallback: traverse tree and for each symbol that is the layer object, collect annotations. */
  private def collectDependsOnFromSymbols(tree: Tree, sourceContent: Option[String] = None)(using Context): Unit =
    tree.foreachSubTree { t =>
      if t.symbol.exists then
        val sym = t.symbol
        if sym.name.toString == "layer" && sym.is(Flags.ModuleVal) then
          val pkgName = packageOf(sym.owner)
          if pkgName.nonEmpty then
            val allowed = extractDependsOnFromSymbol(t.symbol)
            packageToAllowed(pkgName) = packageToAllowed.getOrElse(pkgName, Set.empty) ++ allowed
            if !packageToHashHex.contains(pkgName) then packageToHashHex(pkgName) = dependsOnHash(allowed, sourceContent)
    }

  /** Hash of @dependsOn content + source for Zinc invalidation. Changes when layer file or @dependsOn changes. */
  private def dependsOnHash(allowed: Set[String], sourceContent: Option[String] = None): String =
    val depContent = if allowed.isEmpty then "" else allowed.toVector.sorted.mkString(",")
    val srcContent = sourceContent.getOrElse("")
    val content = depContent + "|" + srcContent
    val digest = MessageDigest.getInstance("SHA-256").digest(content.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    digest.take(8).map(b => "%02x".format(b & 0xff)).mkString

  /** Whether the owner is the layer object's module class. */
  private def isLayerObject(owner: Symbol)(using Context): Boolean =
    owner.is(Flags.ModuleClass) && owner.sourceModule.exists && owner.sourceModule.name.toString == "layer"

  /** Resolve the layer object for a package. Tries classpath first (requiredModule) so we find layers
    * compiled in a previous run, then falls back to package decls.
    */
  private def resolveLayerSymbol(pkgName: String)(using Context): Symbol | Null =
    def tryClasspath: Symbol | Null =
      try
        val lm = requiredModule(s"$pkgName.layer")
        if lm.exists then lm else null
      catch case _: Exception => null
    def tryFromPackage: Symbol | Null =
      try
        val pkgSym = requiredPackage(pkgName)
        val pkgClass = pkgSym.moduleClass
        if !pkgClass.exists then null
        else
          pkgClass.denot.ensureCompleted()
          pkgClass.info.decls.iterator.find(s => s.name.toString == "layer") match
            case Some(s) => s
            case None    => null
      catch case _: Exception => null
    tryClasspath match
      case s: Symbol if s != null => s
      case _ => tryFromPackage


  /** Extract string constants from annotation args. Handles varargs (String*) which appear as Typed(SeqLiteral(...), <repeated>). */
  private def extractStringsFromAnnotationArgs(args: List[Tree])(using Context): Set[String] =
    args.flatMap {
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

  private def extractDependsOnFromSymbol(sym: Symbol)(using Context): Set[String] =
    val cls = if sym.is(Flags.ModuleVal) then sym.moduleClass else sym.asClass
    if !cls.exists then return Set.empty
    val annots = cls.annotations
    val dependsOnAnnot = annots.find(a => a.symbol.fullName.toString.endsWith("dependsOn"))
    dependsOnAnnot match
      case None => Set.empty
      case Some(annot) =>
        extractStringsFromAnnotationArgs(annot.arguments)

  private def isAllowed(ownerPackage: String, refPackage: String): Boolean =
    if refPackage.isEmpty || refPackage == "root" then true
    else if isStdLib(refPackage) then true
    else if ownerPackage == refPackage then true
    else if refPackage.startsWith(ownerPackage + ".") then true
    else
      val allowed = packageToAllowed.getOrElse(ownerPackage, Set.empty) + "scala" + "java"
      allowed.exists(prefix => refPackage == prefix || refPackage.startsWith(prefix + "."))

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    val ownerSym = tree.symbol
    if ownerSym.is(Flags.Synthetic) then return tree

    val ownerPackage = packageOf(ownerSym)

    val refs = collectTypeRefs(tree.rhs)
    recordFileDepsFromTree(tree.rhs)

    for (refPkg, pos) <- refs do
      if !isStdLib(refPkg) then
        if !isAllowed(ownerPackage, refPkg) then
          report.error(
            s"Package $ownerPackage cannot depend on $refPkg. " +
              s"Add @dependsOn(\"$refPkg\") to the $ownerPackage layer object to allow this dependency.",
            pos
          )

    tree

  override def transformTemplate(tree: Template)(using Context): Tree =
    val owner = tree.symbol.owner
    if owner.exists && owner.isClass then
      for paramList <- tree.constr.paramss; param <- paramList do
        param match
          case v: ValDef if v.tpt.tpe.exists =>
            checkDependencies(owner, v.tpt.tpe, v.srcPos)
          case _ =>
      val ownerPkg = packageOf(owner)
      val hashHexOpt = packageToHashHex.get(ownerPkg)
      val additions: List[ValDef] =
        if isLayerObject(owner) && hashHexOpt.isDefined then
          val hashName = termName(s"hash_${hashHexOpt.get}")
          val hashSym = newSymbol(owner, hashName, Flags.Synthetic, defn.IntType).asTerm
          hashSym.entered
          val hashValDef = ValDef(hashSym, Literal(Constant(1)))
          List(hashValDef)
        else if !isLayerObject(owner) then
          try
            val layerObjSym = resolveLayerSymbol(ownerPkg)
            if layerObjSym != null then
              layerObjSym.denot.ensureCompleted()
              val layerType = if layerObjSym.info.exists then layerObjSym.info else defn.AnyType
              val layerRef = ref(layerObjSym)
              // Reference the layer object so Zinc invalidates this class when layer changes (@dependsOn).
              // Fetched from classpath when layer.scala was not compiled in this run.
              val layerRefSym =
                newSymbol(owner, termName("_layerRef"), Flags.Synthetic | Flags.Private | Flags.Lazy, layerType).asTerm
              layerRefSym.entered
              val ownLayerRefs = List(ValDef(layerRefSym, layerRef))
              val dependentLayerRefs = collectDependentLayerRefs(owner, tree)
              ownLayerRefs ++ dependentLayerRefs
            else
              // No layer object: use special hash instead of _layerRef. Zinc semantics for "no layers".
              val hashSym =
                newSymbol(owner, termName(s"hash_$noLayersHash"), Flags.Synthetic, defn.IntType).asTerm
              hashSym.entered
              List(ValDef(hashSym, Literal(Constant(1))))
          catch case _: Exception => Nil
        else Nil
      if additions.nonEmpty then
        val newBody = tree.body ++ additions
        cpy.Template(tree)(body = newBody)
      else tree
    else tree

  override def transformValDef(tree: ValDef)(using Context): Tree =
    if tree.symbol.exists && tree.symbol.owner.exists && tree.symbol.owner.isClass && !tree.symbol.is(Flags.Synthetic) then
      checkDependencies(tree.symbol.owner, tree.tpt.tpe, tree.srcPos)
    tree

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    val isConstructor = tree.symbol.isConstructor
    if tree.symbol.exists && tree.symbol.owner.exists && tree.symbol.owner.isClass &&
       (isConstructor || !tree.symbol.isOneOf(Flags.Synthetic | Flags.Deferred)) then
      checkDependencies(tree.symbol.owner, tree.tpt.tpe, tree.srcPos)
      for paramList <- tree.paramss; param <- paramList do
        param match
          case v: ValDef if v.tpt.tpe.exists =>
            checkDependencies(tree.symbol.owner, v.tpt.tpe, tree.srcPos)
          case _ =>
      tree.rhs match
        case rhs: Tree if !rhs.isEmpty => checkTreeDeps(tree.symbol.owner, rhs, tree.srcPos)
        case _ =>
    tree

  private def checkDependencies(owner: Symbol, tpe: Type, pos: dotty.tools.dotc.util.SrcPos)(using Context): Unit =
    val ownerPkg = packageOf(owner)
    recordFileDepsFromType(owner, tpe, pos)
    for refPkg <- packagesInType(tpe) do
      if !isStdLib(refPkg) && !isAllowed(ownerPkg, refPkg) then
        report.error(
          s"Package $ownerPkg cannot depend on $refPkg. " +
            s"Add @dependsOn(\"$refPkg\") to the $ownerPkg layer object to allow this dependency.",
          pos
        )

  private def checkTreeDeps(owner: Symbol, tree: Tree, pos: dotty.tools.dotc.util.SrcPos)(using Context): Unit =
    val ownerPkg = packageOf(owner)
    recordFileDepsFromTree(tree)
    for (refPkg, refPos) <- collectTypeRefs(tree) do
      if !isStdLib(refPkg) && !isAllowed(ownerPkg, refPkg) then
        report.error(
          s"Package $ownerPkg cannot depend on $refPkg. " +
            s"Add @dependsOn(\"$refPkg\") to the $ownerPkg layer object to allow this dependency.",
          refPos
        )

  private def packageOf(sym: Symbol)(using Context): String =
    def loop(s: Symbol): String =
      if s == null || !s.exists || s.isRoot || s == defn.RootPackage then ""
      else if s.is(Flags.Package) then
        val rest = loop(s.owner)
        if rest.isEmpty then s.name.toString else s"$rest.${s.name}"
      else loop(s.owner)
    val pkg = loop(sym)
    if pkg.isEmpty then "root" else pkg

  private def packagesInType(tpe: Type)(using Context): Set[String] =
    tpe match
      case ref: TypeRef =>
        val sym = ref.symbol
        if sym.exists && !sym.isPrimitiveValueClass then Set(packageOf(sym))
        else Set.empty
      case app: AppliedType =>
        packagesInType(app.tycon) ++ app.args.flatMap(packagesInType).toSet
      case and: AndType =>
        packagesInType(and.tp1) ++ packagesInType(and.tp2)
      case or: OrType =>
        packagesInType(or.tp1) ++ packagesInType(or.tp2)
      case _ => Set.empty

  /** Collect packages referenced from a template (constructor params and body) for Zinc invalidation. */
  private def collectPackagesFromTemplate(template: Template, ownerPkg: String)(using Context): Set[String] =
    val fromConstr = collectTypeRefs(template.constr)
    val fromBody = template.body.flatMap(collectTypeRefs)
    val refs = fromConstr ++ fromBody
    refs.map(_._1).filter(p => p.nonEmpty && p != "root" && !isStdLib(p) && p != ownerPkg && !p.startsWith(ownerPkg + ".")).toSet

  /** Add _layerRef_<pkg> for each dependent package so Zinc invalidates when those layers change. */
  private def collectDependentLayerRefs(owner: Symbol, template: Template)(using Context): List[ValDef] =
    val ownerPkg = packageOf(owner)
    val refPkgs = collectPackagesFromTemplate(template, ownerPkg)
    val buf = List.newBuilder[ValDef]
    for refPkg <- refPkgs.toVector.sorted do
      try
        val layerObjSym = resolveLayerSymbol(refPkg)
        if layerObjSym != null then
          layerObjSym.denot.ensureCompleted()
          val layerType = if layerObjSym.info.exists then layerObjSym.info else defn.AnyType
          val layerRef = ref(layerObjSym)
          val safeName = refPkg.replace(".", "_")
          val layerRefSym =
            newSymbol(owner, termName(s"_layerRef_$safeName"), Flags.Synthetic | Flags.Private | Flags.Lazy, layerType).asTerm
          layerRefSym.entered
          buf += ValDef(layerRefSym, layerRef)
      catch case _: Exception => ()
    buf.result()

  private def collectTypeRefs(tree: Tree)(using Context): List[(String, dotty.tools.dotc.util.SrcPos)] =
    val buf = List.newBuilder[(String, dotty.tools.dotc.util.SrcPos)]
    tree.foreachSubTree { t =>
      if t.tpe.exists then
        for pkg <- packagesInType(t.tpe) do
          buf += ((pkg, t.srcPos))
    }
    buf.result()

  private def isStdLib(pkg: String): Boolean =
    pkg.startsWith("scala.") || pkg.startsWith("java.") || pkg == "scala" || pkg == "java"

  private def currentSourceFile(using ctx: Context): Option[String] =
    val unit = ctx.compilationUnit
    if unit != null && unit.source != null && unit.source.exists then
      Some(unit.source.file.path)
    else None

  private def sourceFileOf(sym: Symbol)(using Context): Option[String] =
    if !sym.exists then return None
    val topLevel = sym.topLevelClass
    if !topLevel.exists then return None
    val info = topLevel.compilationUnitInfo
    if info == null then return None
    val file = info.associatedFile
    if file == null then None else Some(file.path)

  private def symbolsInType(tpe: Type)(using Context): Set[Symbol] =
    tpe match
      case ref: TypeRef =>
        val sym = ref.symbol
        if sym.exists && !sym.isPrimitiveValueClass then Set(sym) else Set.empty
      case app: AppliedType =>
        symbolsInType(app.tycon) ++ app.args.flatMap(symbolsInType).toSet
      case and: AndType =>
        symbolsInType(and.tp1) ++ symbolsInType(and.tp2)
      case or: OrType =>
        symbolsInType(or.tp1) ++ symbolsInType(or.tp2)
      case _ => Set.empty

  private def recordFileDepsFromType(owner: Symbol, tpe: Type, pos: dotty.tools.dotc.util.SrcPos)(using Context): Unit =
    for
      fromFile <- currentSourceFile
      sym <- symbolsInType(tpe)
      toFile <- sourceFileOf(sym)
      if !isStdLib(packageOf(sym))
    do
      recordFileDep(fromFile, toFile, pos)

  private def recordFileDepsFromTree(tree: Tree)(using Context): Unit =
    tree.foreachSubTree { t =>
      if t.tpe.exists then
        for sym <- symbolsInType(t.tpe) do
          for
            fromFile <- currentSourceFile
            toFile <- sourceFileOf(sym)
            if !isStdLib(packageOf(sym))
          do
            recordFileDep(fromFile, toFile, t.srcPos)
    }

  private def recordFileDep(from: String, to: String, pos: dotty.tools.dotc.util.SrcPos)(using ctx: Context): Unit =
    if from == to then return
    val deps = fileDeps.getOrElseUpdate(from, mutable.Set())
    if deps.add(to) then
      if hasPath(to, from) then
        report.error(
          s"Cycle between files: $from and $to depend on each other. Cycles between files are not allowed.",
          pos
        )

  private def hasPath(from: String, to: String): Boolean =
    val visited = mutable.Set[String]()
    def dfs(current: String): Boolean =
      if current == to then true
      else if visited(current) then false
      else
        visited += current
        fileDeps.getOrElse(current, mutable.Set()).exists(dfs)
    dfs(from)
