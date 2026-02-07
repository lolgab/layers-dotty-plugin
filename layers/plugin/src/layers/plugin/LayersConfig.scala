package layers.plugin

/** Configuration for layer enforcement.
  *
  * Layers are ordered from innermost (0) to outermost. In onion architecture:
  * - Inner layers cannot depend on outer layers
  * - Outer layers can depend on inner layers
  *
  * Config format: -P:layers:config=path (loaded from classpath resources). Config file is required and must exist in resources.
  * Allowed imports: -P:layers:allowed=domain:scala.;domain:java.
  * Strict mode: -P:layers:strict=true or strict: true in config. When enabled, outer layers may only depend on direct inner neighbors.
  * Max fan-out: -P:layers:maxFanOut=10 or maxFanOut: 10 in config. Limits distinct packages per file (stdlib excluded).
  * Max layers: -P:layers:maxLayers=5 or maxLayers: 5 in config. Limits the maximum number of layers allowed in the application.
  * Domain purity: -P:layers:domainPurity=true or domainPurity: true in config. When enabled, domain layer cannot use effect types (Future, IO, Task) or framework annotations (javax.*, jakarta.*, org.springframework.*, etc.).
  * Naming conventions: conventions.ports and conventions.adapters in config. When specified, warns when traits in domain/application don't match port suffixes, or classes in outermost layer don't match adapter suffixes.
  * Dependency inversion: dependencyInversionWarning=true or -P:layers:dependencyInversionWarning=true. When enabled, warns when an outer layer references a concrete class from an inner layer (prefer depending on a trait/interface instead).
  * Parallel layers: use parallel: section to group layers that share a tier but cannot depend on each other (e.g. db, views).
  * Bounded context (slices): slices define vertical module boundaries. Cross-slice dependencies require explicit allowlist in cross-slice.
  * When allowed is specified for a layer, only those package prefixes may be used; everything else is forbidden.
  * The layer itself and all inner layers are always allowed and need not be listed.
  */
case class LayersConfig(
  layers: List[String],
  packageToLayer: Map[String, Int],
  layerToTier: Map[String, Int],
  allowedByLayer: Map[Int, List[String]] = Map.empty,
  strict: Boolean = false,
  maxFanOut: Option[Int] = None,
  maxLayers: Option[Int] = None,
  domainPurity: Boolean = false,
  portSuffixes: List[String] = Nil,
  adapterSuffixes: List[String] = Nil,
  slices: Map[String, List[String]] = Map.empty,
  crossSliceAllowed: Set[(String, String)] = Set.empty,
  dependencyInversionWarning: Boolean = false
):
  /** Returns (tier, layerName) for the package. Tier 0 is innermost. */
  def layerForPackage(pkg: String): Option[(Int, String)] =
    val matches = packageToLayer.filter { case (layerName, _) =>
      pkg == layerName ||
      pkg.startsWith(layerName + ".") ||
      pkg.endsWith("." + layerName) ||
      pkg.contains("." + layerName + ".")
    }
    if matches.isEmpty then None
    else
      val layerName = matches.maxBy(_._1.length)._1
      layerToTier.get(layerName).map(tier => (tier, layerName))

  /** Returns true if a dependency from (fromTier, fromLayer) to (toTier, toLayer) is allowed. */
  def isAllowed(fromTier: Int, fromLayer: String, toTier: Int, toLayer: String): Boolean =
    if fromTier < toTier then false
    else if fromTier > toTier then
      if strict then fromTier == toTier + 1 else true
    else
      fromLayer == toLayer

  /** Human-readable reason when isAllowed is false. */
  def violationReason(fromTier: Int, fromLayer: String, toTier: Int, toLayer: String): String =
    if fromTier < toTier then "Inner layers must not depend on outer layers."
    else if fromTier == toTier && fromLayer != toLayer then
      s"Parallel layers $fromLayer and $toLayer cannot depend on each other."
    else if strict then
      val directNeighbors = layers.filter(l => layerToTier.get(l).contains(fromTier - 1))
      val neighbor = directNeighbors.mkString(", ")
      s"Strict mode: cannot skip layers. $fromLayer may only depend on direct neighbor(s) $neighbor, not $toLayer."
    else "Unknown violation"

  /** The outermost tier index. */
  def outermostTier: Int = layerToTier.values.maxOption.getOrElse(0)

  /** Domain layer tier when domainPurity is enabled. */
  def domainLayerTier: Option[Int] =
    if domainPurity then layerToTier.get("domain")
    else None

  /** Forbidden package prefixes when domain purity is enabled for the domain layer. */
  def domainPurityForbidden: List[String] = List(
    "scala.concurrent.",
    "cats.effect.",
    "zio.",
    "monix.",
    "javax.",
    "jakarta.",
    "org.springframework.",
    "com.fasterxml.jackson.",
    "io.circe.",
    "org.http4s.",
    "play.api.",
    "akka."
  )

  /** Returns Some(violatingPackage) if refPackage matches domain purity forbidden list and owner is in domain layer. */
  def domainPurityViolation(layerTier: Int, refPackage: String): Option[String] =
    domainLayerTier.filter(_ == layerTier).flatMap { _ =>
      domainPurityForbidden.find { prefix =>
        val base = prefix.stripSuffix(".")
        refPackage == base || refPackage.startsWith(base + ".")
      }
    }

  /** Returns true if conventions are enabled (port or adapter suffixes specified). */
  def conventionsEnabled: Boolean = portSuffixes.nonEmpty || adapterSuffixes.nonEmpty

  /** Returns true if pkg matches any port suffix (e.g. domain.port, domain.user.port, com.app.ports). */
  def packageMatchesPortConvention(pkg: String): Boolean =
    portSuffixes.exists { suffix =>
      val s = suffix.trim
      val base = if s.startsWith(".") then s.drop(1) else s
      pkg == base || pkg.endsWith("." + base) || pkg.contains("." + base + ".")
    }

  /** Returns true if pkg matches any adapter suffix (e.g. infrastructure.adapter, com.app.infrastructure). */
  def packageMatchesAdapterConvention(pkg: String): Boolean =
    adapterSuffixes.exists { suffix =>
      val s = suffix.trim
      val base = if s.startsWith(".") then s.drop(1) else s
      pkg == base || pkg.endsWith("." + base) || pkg.contains("." + base + ".")
    }

  /** Returns the slice name for a package, if slices are configured. Longest matching prefix wins. */
  def sliceForPackage(pkg: String): Option[String] =
    if slices.isEmpty then None
    else
      val matches = slices.flatMap { case (sliceName, prefixes) =>
        prefixes.flatMap { prefix =>
          val base = prefix.stripSuffix(".")
          if pkg == base || pkg.startsWith(base + ".") then Some((sliceName, base.length))
          else None
        }
      }
      if matches.isEmpty then None
      else Some(matches.maxBy(_._2)._1)

  /** Returns true if cross-slice dependency from fromSlice to toSlice is allowed. */
  def isCrossSliceAllowed(fromSlice: String, toSlice: String): Boolean =
    fromSlice == toSlice || crossSliceAllowed((fromSlice, toSlice))

  /** Returns Some((fromSlice, toSlice)) if cross-slice violation: fromPkg depends on toPkg across slices without allowlist. */
  def crossSliceViolation(fromPkg: String, toPkg: String): Option[(String, String)] =
    sliceForPackage(fromPkg).flatMap { fromSlice =>
      sliceForPackage(toPkg).flatMap { toSlice =>
        if isCrossSliceAllowed(fromSlice, toSlice) then None
        else Some((fromSlice, toSlice))
      }
    }

  /** Returns Some(violatingPackage) if the layer has an allowed list and refPackage does not match any allowed prefix. */
  def disallowedImport(layerTier: Int, layerName: String, refPackage: String): Option[String] =
    layerForPackage(refPackage) match
      case Some((refTier, _)) if refTier <= layerTier => None
      case _ =>
        val prefixes = allowedByLayer.get(layers.indexOf(layerName)).orElse(Some(List("scala.", "java."))).filter(_.nonEmpty)
        prefixes.flatMap { p =>
          val matches = p.exists { prefix =>
            val base = prefix.stripSuffix(".")
            refPackage == base || refPackage.startsWith(base + ".")
          }
          if matches then None else Some(refPackage)
        }

object LayersConfig:
  /** Error message when layer count exceeds maxLayers limit. */
  def maxLayersExceededMessage(layerCount: Int, limit: Int): String =
    val layersToRemove = layerCount - limit
    val layerOrLayers = if layersToRemove == 1 then "layer" else "layers"
    s"has $layerCount layers but maxLayers is $limit. Remove $layersToRemove $layerOrLayers or increase maxLayers."

  /** Returns Right(config) on success, Left(errorMessage) on failure. */
  def parse(options: List[String]): Either[String, LayersConfig] =
    parseEither(options)

  /** Like parse but throws on failure. Use in tests; the plugin uses parse and report.error instead. */
  def parseOrThrow(options: List[String]): LayersConfig =
    parse(options).fold(err => throw new IllegalArgumentException(err), identity)

  private def parseEither(options: List[String]): Either[String, LayersConfig] =
    val configPath = options.collectFirst { case s if s.startsWith("config=") => s.drop(7) }
    val allowedOpt = options.collectFirst { case s if s.startsWith("allowed=") => s.drop(8) }
    val strictFromOptions = options.collectFirst { case s if s.startsWith("strict=") => s.drop(7).toLowerCase == "true" }

    val maxFanOutFromOptions = options.collectFirst { case s if s.startsWith("maxFanOut=") => s.drop(10).trim.toIntOption }.flatten.filter(_ > 0)
    val maxLayersFromOptions = options.collectFirst { case s if s.startsWith("maxLayers=") => s.drop(10).trim.toIntOption }.flatten.filter(_ > 0)
    val domainPurityFromOptions = options.collectFirst { case s if s.startsWith("domainPurity=") => s.drop(12).toLowerCase == "true" }
    val dependencyInversionWarningFromOptions = options.collectFirst { case s if s.startsWith("dependencyInversionWarning=") => s.drop(27).toLowerCase == "true" }

    val configPathRequired = configPath match
      case None =>
        return Left("layers plugin requires config=path (e.g. -P:layers:config=layers.conf). Place layers.conf in your project's resources.")
      case Some(p) => p
    val content = loadConfig(configPathRequired) match
      case None =>
        return Left(
          s"layers plugin: config file not found: $configPathRequired. " +
            "Place layers.conf in your project's resources (e.g. src/main/resources/ or resources/) so it is on the compile classpath. " +
            "Alternatively, use an absolute path: -P:layers:config=/path/to/layers.conf"
        )
      case Some(c) => c
    val parsed = parseConfigContent(content)
    if parsed.layers.isEmpty then
      return Left(s"layers plugin: config file $configPathRequired has no layers defined")

    val (layers, allowedFromConfig, strictFromConfig, maxFanOutFromConfig, maxLayersFromConfig, domainPurityFromConfig, portSuffixesFromConfig, adapterSuffixesFromConfig, slicesFromConfig, crossSliceFromConfig, parallelGroupsFromConfig, dependencyInversionWarningFromConfig) =
      (parsed.layers, parsed.allowedByLayerName, parsed.strict, parsed.maxFanOut, parsed.maxLayers, parsed.domainPurity, parsed.portSuffixes, parsed.adapterSuffixes, parsed.slices, parsed.crossSliceAllowed, parsed.parallelGroups, parsed.dependencyInversionWarning)

    val strict = strictFromOptions.getOrElse(strictFromConfig)

    val allowedFromOptions = allowedOpt.map(parseAllowedOption).getOrElse(Map.empty[String, List[String]])
    val allowedByLayerName = mergeAllowed(allowedFromConfig, allowedFromOptions)
    val packageToLayer = layers.zipWithIndex.toMap
    val layerToTier = buildLayerToTier(layers, parallelGroupsFromConfig)
    val allowedByLayer = (0 until layers.size).flatMap { idx =>
      val layerName = layers(idx)
      allowedByLayerName.get(layerName) match
        case Some(p) if p.nonEmpty => Some(idx -> p)
        case _                     => Some(idx -> List("scala.", "java."))
    }.toMap

    val maxFanOut = maxFanOutFromOptions.orElse(maxFanOutFromConfig)
    val maxLayers = maxLayersFromOptions.orElse(maxLayersFromConfig)
    val domainPurity = domainPurityFromOptions.getOrElse(domainPurityFromConfig)

    maxLayers.foreach { limit =>
      if layers.size > limit then
        return Left(s"layers plugin: application ${maxLayersExceededMessage(layers.size, limit)}")
    }
    val portSuffixes = portSuffixesFromConfig
    val adapterSuffixes = adapterSuffixesFromConfig
    val slices = slicesFromConfig
    val crossSliceAllowed = crossSliceFromConfig
    val dependencyInversionWarning = dependencyInversionWarningFromOptions.getOrElse(dependencyInversionWarningFromConfig)

    Right(LayersConfig(layers, packageToLayer, layerToTier, allowedByLayer, strict, maxFanOut, maxLayers, domainPurity, portSuffixes, adapterSuffixes, slices, crossSliceAllowed, dependencyInversionWarning))

  /** Build layer name -> tier map. Layers in the same parallel group share a tier. */
  private def buildLayerToTier(layers: List[String], parallelGroups: List[Set[String]]): Map[String, Int] =
    val groupByLayer = parallelGroups.flatMap(g => g.map(_ -> g)).toMap
    var currentTier = 0
    var tierByGroup = Map.empty[Set[String], Int]
    layers.flatMap { layer =>
      groupByLayer.get(layer) match
        case Some(group) =>
          val tier = tierByGroup.get(group) match
            case Some(t) => t
            case None =>
              val t = currentTier
              currentTier += 1
              tierByGroup = tierByGroup + (group -> t)
              t
          Some(layer -> tier)
        case None =>
          val tier = currentTier
          currentTier += 1
          Some(layer -> tier)
    }.toMap

  private case class ParsedConfig(
      layers: List[String],
      allowedByLayerName: Map[String, List[String]],
      strict: Boolean,
      maxFanOut: Option[Int] = None,
      maxLayers: Option[Int] = None,
      domainPurity: Boolean = false,
      portSuffixes: List[String] = Nil,
      adapterSuffixes: List[String] = Nil,
      slices: Map[String, List[String]] = Map.empty,
      crossSliceAllowed: Set[(String, String)] = Set.empty,
      parallelGroups: List[Set[String]] = Nil,
      dependencyInversionWarning: Boolean = false
  )

  /** Parse allowed=application:domain;infrastructure:domain,application into Map(packageName -> List(prefixes)).
    * Uses semicolon as top-level delimiter (comma may be split by scalac). */
  def parseAllowedOption(value: String): Map[String, List[String]] =
    value.split("[;,]").iterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap { part =>
        part.split(":", 2) match
          case Array(layer, prefixes) => Some(layer.trim -> prefixes.split(",").map(_.trim).filter(_.nonEmpty).toList)
          case _                      => None
      }
      .toList
      .groupMapReduce(_._1)(_._2)(_ ++ _)

  def mergeAllowed(
      fromConfig: Map[String, List[String]],
      fromOptions: Map[String, List[String]]
  ): Map[String, List[String]] =
    fromConfig ++ fromOptions.map { case (k, v) => k -> (fromConfig.getOrElse(k, Nil) ++ v).distinct }

  /** Load config from classpath resources or file path. Tries multiple classloaders, then falls back to filesystem. */
  private def loadConfig(path: String): Option[String] =
    val resourcePath = path.replace('\\', '/').stripPrefix("/")
    def fromClassloader(cl: ClassLoader): Option[String] =
      Option(cl).flatMap { c =>
        Option(c.getResourceAsStream(resourcePath)).flatMap { in =>
          scala.util.Using(scala.io.Source.fromInputStream(in))(_.mkString).toOption
        }
      }
    def fromFile: Option[String] =
      val f = java.io.File(path.replace('/', java.io.File.separatorChar))
      if f.isFile then scala.util.Using(scala.io.Source.fromFile(f))(_.mkString).toOption else None
    fromClassloader(Thread.currentThread().getContextClassLoader)
      .orElse(fromClassloader(getClass.getClassLoader))
      .orElse(fromClassloader(ClassLoader.getSystemClassLoader))
      .orElse(fromFile)

  private def parseConfigContent(content: String): ParsedConfig =
    val lines = content.linesIterator.toList
    val (layers, allowedByLayerName, strict, maxFanOut, maxLayers, domainPurity, portSuffixes, adapterSuffixes, slices, crossSliceAllowed, parallelGroups, dependencyInversionWarning, _, _) = lines.foldLeft(
      (List.empty[String], Map.empty[String, List[String]], false, Option.empty[Int], Option.empty[Int], false, List.empty[String], List.empty[String], Map.empty[String, List[String]], Set.empty[(String, String)], List.empty[Set[String]], false, "", Option.empty[String])
    ) {
      case ((accLayers, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, section, currentLayer), rawLine) =>
        val line = rawLine.trim
        if line.isEmpty || line.startsWith("#") then (accLayers, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, section, currentLayer)
        else if (rawLine.startsWith(" ") || rawLine.startsWith("\t")) && line.startsWith("allowed:") then
          val prefixes = line.drop(8).split(",").map(_.trim).filter(_.nonEmpty).toList
          currentLayer match
            case Some(layer) =>
              val merged = accAllowed.getOrElse(layer, Nil) ++ prefixes
              (accLayers, accAllowed + (layer -> merged), accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, "", currentLayer)
            case None => (accLayers, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, section, currentLayer)
        else if (rawLine.startsWith(" ") || rawLine.startsWith("\t")) && section == "conventions" && line.startsWith("ports:") then
          val suffixes = line.drop(6).split(",").map(_.trim).filter(_.nonEmpty).toList
          (accLayers, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes ++ suffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, section, currentLayer)
        else if (rawLine.startsWith(" ") || rawLine.startsWith("\t")) && section == "conventions" && line.startsWith("adapters:") then
          val suffixes = line.drop(9).split(",").map(_.trim).filter(_.nonEmpty).toList
          (accLayers, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes ++ suffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, section, currentLayer)
        else if (rawLine.startsWith(" ") || rawLine.startsWith("\t")) && section == "slices" && line.contains(":") then
          val idx = line.indexOf(':')
          val sliceName = line.take(idx).trim
          val prefixes = line.drop(idx + 1).split(",").map(_.trim).filter(_.nonEmpty).toList
          (accLayers, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices + (sliceName -> prefixes), accCrossSlice, accParallel, accDepInvWarn, section, currentLayer)
        else if (rawLine.startsWith(" ") || rawLine.startsWith("\t")) && section == "cross-slice" && line.contains("->") then
          val parts = line.split("->", 2).map(_.trim)
          if parts.length == 2 && parts(0).nonEmpty && parts(1).nonEmpty then
            (accLayers, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice + ((parts(0), parts(1))), accParallel, accDepInvWarn, section, currentLayer)
          else (accLayers, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, section, currentLayer)
        else if (rawLine.startsWith(" ") || rawLine.startsWith("\t")) && section == "parallel" then
          val layerNames = line.split(",").map(_.trim).filter(_.nonEmpty).toSet
          if layerNames.size >= 2 then
            (accLayers, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel :+ layerNames, accDepInvWarn, section, currentLayer)
          else (accLayers, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, section, currentLayer)
        else if !rawLine.startsWith(" ") && !rawLine.startsWith("\t") then
          val lower = line.toLowerCase
          if lower == "conventions" || lower.startsWith("conventions:") then
            (accLayers, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, "conventions", currentLayer)
          else if lower == "slices" || lower.startsWith("slices:") then
            (accLayers, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, "slices", currentLayer)
          else if lower == "cross-slice" || lower.startsWith("cross-slice:") then
            (accLayers, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, "cross-slice", currentLayer)
          else if lower == "parallel" || lower.startsWith("parallel:") then
            (accLayers, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, "parallel", currentLayer)
          else if lower == "strict" || lower.startsWith("strict:") then
            val strictVal = lower == "strict" || line.drop(7).trim.toLowerCase == "true"
            (accLayers, accAllowed, accStrict || strictVal, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, "", currentLayer)
          else if lower.startsWith("maxfanout:") then
            val value = line.drop(10).trim
            val parsed = value.toIntOption.filter(_ > 0)
            (accLayers, accAllowed, accStrict, parsed.orElse(accMaxFanOut), accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, "", currentLayer)
          else if lower == "domainpurity" || lower.startsWith("domainpurity:") then
            val purityVal = lower == "domainpurity" || line.drop(12).trim.toLowerCase == "true"
            (accLayers, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity || purityVal, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, "", currentLayer)
          else if lower.startsWith("maxlayers:") then
            val value = line.drop(10).trim
            val parsed = value.toIntOption.filter(_ > 0)
            (accLayers, accAllowed, accStrict, accMaxFanOut, parsed.orElse(accMaxLayers), accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, "", currentLayer)
          else if lower == "dependencyinversionwarning" || lower.startsWith("dependencyinversionwarning:") then
            val warnVal = lower == "dependencyinversionwarning" || line.drop(27).trim.toLowerCase == "true"
            (accLayers, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn || warnVal, "", currentLayer)
          else
            val layer = line
            (accLayers :+ layer, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, "", Some(layer))
        else (accLayers, accAllowed, accStrict, accMaxFanOut, accMaxLayers, accDomainPurity, accPortSuffixes, accAdapterSuffixes, accSlices, accCrossSlice, accParallel, accDepInvWarn, section, currentLayer)
    }
    ParsedConfig(layers, allowedByLayerName, strict, maxFanOut, maxLayers, domainPurity, portSuffixes, adapterSuffixes, slices, crossSliceAllowed, parallelGroups, dependencyInversionWarning)
