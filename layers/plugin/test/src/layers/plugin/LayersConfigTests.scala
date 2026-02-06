package layers.plugin

import munit.FunSuite

class LayersConfigTests extends FunSuite:

  test("layerForPackage matches exact layer name") {
    val config = LayersConfig.parseOrThrow(List("config=layers-strict.conf"))
    assertEquals(config.layerForPackage("domain"), Some((0, "domain")))
    assertEquals(config.layerForPackage("application"), Some((1, "application")))
    assertEquals(config.layerForPackage("infrastructure"), Some((2, "infrastructure")))
  }

  test("layerForPackage matches package containing layer name") {
    val config = LayersConfig.parseOrThrow(List("config=layers-strict.conf"))
    assertEquals(config.layerForPackage("com.example.domain"), Some((0, "domain")))
    assertEquals(config.layerForPackage("com.myapp.application.service"), Some((1, "application")))
    assertEquals(config.layerForPackage("com.myapp.infrastructure.persistence"), Some((2, "infrastructure")))
  }

  test("layerForPackage matches package prefix") {
    val config = LayersConfig.parseOrThrow(List("config=layers-strict.conf"))
    assertEquals(config.layerForPackage("domain.repository"), Some((0, "domain")))
  }

  test("isAllowed: inner can depend on inner or same") {
    val config = LayersConfig.parseOrThrow(List("config=layers-strict.conf"))
    assert(config.isAllowed(0, "domain", 0, "domain"))
    assert(config.isAllowed(1, "application", 0, "domain"))
    assert(config.isAllowed(1, "application", 1, "application"))
    assert(config.isAllowed(2, "infrastructure", 1, "application"))
  }

  test("isAllowed: inner cannot depend on outer") {
    val config = LayersConfig.parseOrThrow(List("config=layers-strict.conf"))
    assert(!config.isAllowed(0, "domain", 1, "application"))
    assert(!config.isAllowed(0, "domain", 2, "infrastructure"))
    assert(!config.isAllowed(1, "application", 2, "infrastructure"))
  }

  test("isAllowed: strict mode allows only direct neighbor or same layer") {
    val config = LayersConfig.parseOrThrow(List("config=layers-strict.conf"))
    assert(config.strict)
    // Same layer
    assert(config.isAllowed(0, "domain", 0, "domain"))
    assert(config.isAllowed(1, "application", 1, "application"))
    assert(config.isAllowed(2, "infrastructure", 2, "infrastructure"))
    // Direct neighbor: application -> domain, infrastructure -> application
    assert(config.isAllowed(1, "application", 0, "domain"))
    assert(config.isAllowed(2, "infrastructure", 1, "application"))
    // Skipping layer: infrastructure -> domain (not allowed)
    assert(!config.isAllowed(2, "infrastructure", 0, "domain"))
    // Inner cannot depend on outer (unchanged)
    assert(!config.isAllowed(0, "domain", 1, "application"))
    assert(!config.isAllowed(0, "domain", 2, "infrastructure"))
    assert(!config.isAllowed(1, "application", 2, "infrastructure"))
  }

  test("parse strict from inline option") {
    val config = LayersConfig.parseOrThrow(List("config=layers-strict.conf"))
    assert(config.strict)
  }

  test("parse strict from config file") {
    val config = LayersConfig.parseOrThrow(List("config=layers-strict.conf"))
    assert(config.strict)
    assertEquals(config.layers, List("domain", "application", "infrastructure"))
  }

  test("parse fails when config file not found") {
    val result = LayersConfig.parse(List("config=nonexistent.conf"))
    assert(result.isLeft)
    assert(result.fold(_.contains("config file not found"), _ => false))
  }

  test("parse fails when config=path not specified") {
    val result = LayersConfig.parse(List("strict=true"))
    assert(result.isLeft)
    assert(result.fold(_.contains("requires config=path"), _ => false))
  }

  test("parse fails when config file has no layers defined") {
    val result = LayersConfig.parse(List("config=layers-empty.conf"))
    assert(result.isLeft)
    assert(result.fold(_.contains("has no layers defined"), _ => false))
  }

  test("parse loads config from classpath resources") {
    val config = LayersConfig.parseOrThrow(List("config=layers.conf"))
    assertEquals(config.layers, List("core", "service", "api"))
    assertEquals(config.layerForPackage("core"), Some((0, "core")))
    assertEquals(config.layerForPackage("service"), Some((1, "service")))
    assertEquals(config.layerForPackage("api"), Some((2, "api")))
  }

  test("parse loads config from absolute file path when not on classpath") {
    val tempFile = java.nio.file.Files.createTempFile("layers-", ".conf")
    try
      java.nio.file.Files.writeString(tempFile, "domain\napplication\ninfrastructure")
      val config = LayersConfig.parseOrThrow(List(s"config=${tempFile.toAbsolutePath}"))
      assertEquals(config.layers, List("domain", "application", "infrastructure"))
    finally java.nio.file.Files.deleteIfExists(tempFile)
  }

  test("parse loads allowed imports from config file") {
    val config = LayersConfig.parseOrThrow(List("config=layers-allowed.conf"))
    assertEquals(config.layers, List("domain", "application", "infrastructure"))
    assertEquals(config.allowedByLayer.get(0).get, List("scala.", "java."))
    assertEquals(config.allowedByLayer.get(1).get, List("scala.", "java."))
    assert(config.allowedByLayer.get(2).get.contains("scala."))
  }

  test("disallowedImport rejects package not in allowed list") {
    val config = LayersConfig.parseOrThrow(List("config=layers-allowed.conf"))
    assertEquals(config.disallowedImport(0, "domain", "javax.servlet"), Some("javax.servlet"))
    assertEquals(config.disallowedImport(0, "domain", "external"), Some("external"))
    assertEquals(config.disallowedImport(0, "domain", "domain"), None)
    assertEquals(config.disallowedImport(0, "domain", "scala.Predef"), None)
  }

  test("parse allowed from inline option") {
    val config = LayersConfig.parseOrThrow(List("config=layers-allowed.conf", "allowed=domain:scala.,domain:java.,domain:domain."))
    assertEquals(config.allowedByLayer.get(0).get, List("scala.", "java.", "domain."))
  }

  test("parse maxFanOut from inline option") {
    val config = LayersConfig.parseOrThrow(List("config=layers-allowed.conf", "maxFanOut=10"))
    assertEquals(config.maxFanOut, Some(10))
  }

  test("parse maxFanOut from config file") {
    val config = LayersConfig.parseOrThrow(List("config=layers-fanout.conf"))
    assertEquals(config.maxFanOut, Some(10))
    assertEquals(config.layers, List("domain", "application", "infrastructure"))
  }

  test("parse conventions from config file") {
    val config = LayersConfig.parseOrThrow(List("config=layers-conventions.conf"))
    assert(config.conventionsEnabled)
    assertEquals(config.portSuffixes, List(".port", ".ports", ".api"))
    assertEquals(config.adapterSuffixes, List(".adapter", ".infrastructure"))
    assert(config.packageMatchesPortConvention("domain.port"))
    assert(config.packageMatchesPortConvention("com.app.ports"))
    assert(!config.packageMatchesPortConvention("domain.user"))
    assert(config.packageMatchesAdapterConvention("infrastructure.adapter"))
    assert(config.packageMatchesAdapterConvention("com.app.infrastructure"))
    assert(!config.packageMatchesAdapterConvention("infrastructure.persistence"))
  }

  test("parse slices from config file") {
    val config = LayersConfig.parseOrThrow(List("config=layers-slices.conf"))
    assertEquals(config.slices.keySet, Set("user", "order"))
    assertEquals(config.slices("user"), List("domain.user", "application.user", "infrastructure.user"))
    assertEquals(config.slices("order"), List("domain.order", "application.order", "infrastructure.order"))
    assertEquals(config.sliceForPackage("domain.user"), Some("user"))
    assertEquals(config.sliceForPackage("application.user"), Some("user"))
    assertEquals(config.sliceForPackage("domain.order"), Some("order"))
    assertEquals(config.sliceForPackage("domain"), None)
  }

  test("parse cross-slice allowlist from config file") {
    val config = LayersConfig.parseOrThrow(List("config=layers-slices-cross.conf"))
    assert(config.crossSliceAllowed(("user", "order")))
    assert(config.isCrossSliceAllowed("user", "order"))
    assert(config.isCrossSliceAllowed("user", "user"))
    assert(!config.isCrossSliceAllowed("order", "user"))
  }

  test("parse dependencyInversionWarning from inline option") {
    val config = LayersConfig.parseOrThrow(List("config=layers-allowed.conf", "dependencyInversionWarning=true"))
    assert(config.dependencyInversionWarning)
  }

  test("parse dependencyInversionWarning from config file") {
    val config = LayersConfig.parseOrThrow(List("config=layers-dependency-inversion.conf"))
    assert(config.dependencyInversionWarning)
    assertEquals(config.layers, List("domain", "application", "infrastructure"))
  }

  test("parse parallel layers from config file") {
    val config = LayersConfig.parseOrThrow(List("config=layers-parallel.conf"))
    assertEquals(config.layers, List("domain", "db", "views", "controller"))
    assertEquals(config.layerForPackage("domain"), Some((0, "domain")))
    assertEquals(config.layerForPackage("db"), Some((1, "db")))
    assertEquals(config.layerForPackage("views"), Some((1, "views")))
    assertEquals(config.layerForPackage("controller"), Some((2, "controller")))
  }

  test("isAllowed: parallel layers cannot depend on each other") {
    val config = LayersConfig.parseOrThrow(List("config=layers-parallel.conf"))
    // db and views both at tier 1 - same layer (self) allowed
    assert(config.isAllowed(1, "db", 1, "db"))
    assert(config.isAllowed(1, "views", 1, "views"))
    // Parallel layers cannot depend on each other
    assert(!config.isAllowed(1, "db", 1, "views"))
    assert(!config.isAllowed(1, "views", 1, "db"))
    // Both can use domain
    assert(config.isAllowed(1, "db", 0, "domain"))
    assert(config.isAllowed(1, "views", 0, "domain"))
    // Controller can use everything
    assert(config.isAllowed(2, "controller", 0, "domain"))
    assert(config.isAllowed(2, "controller", 1, "db"))
    assert(config.isAllowed(2, "controller", 1, "views"))
  }
