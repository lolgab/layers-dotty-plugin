# Layers Plugin

A Scala 3 compiler plugin that enforces layered/onion architecture at compile time by validating package dependencies via `@dependsOn` annotations.

## Overview

The plugin ensures that your codebase respects architectural boundaries. By default, no cross-package dependencies are allowed. Each package must declare its allowed dependencies explicitly using a `layer` object annotated with `@dependsOn`.

## Installation

### sbt

```scala
val layersVersion = "x.y.z"
addCompilerPlugin("com.github.lolgab" %%% "layers-plugin" % layersVersion)
libraryDependencies += "com.github.lolgab" %%% "layers" % layersVersion
```

### Mill

Add the plugin to your build:

```scala
val layersVersion = "x.y.z"

def scalacPluginIvyDeps = Seq(mvn"com.github.lolgab:::layers-plugin:$layersVersion")
def ivyDeps = Seq(mvn"com.github.lolgab::layers:$layersVersion")
```

## Usage

### 1. Create a `layer` object in each package

The object must be named `layer`. Use `@dependsOn` to declare which packages this package may depend on:

```scala
package application

@layers.dependsOn("domain")
object layer
```

This allows the `application` package to depend on `domain` and any subpackage (e.g. `domain.user`).

### 2. Declare multiple dependencies

```scala
package infrastructure

@layers.dependsOn("domain", "application")
object layer
```

### Options

- **maxLayers**: Limit the maximum number of layers allowed (e.g. `-P:layers:maxLayers=5`). Compilation fails if the application has more layers than the limit. The layer count is the **height of the dependency tree** (longest path from root to leaf). Only packages with layer objects are counted; external dependencies (e.g. `scala.*`, `java.*`, third-party libraries) are ignored.

### Rules

- **@dependsOn placement**: The annotation may only be placed on `object layer`. It fails if placed on a class, trait, or an object with a different name.
- **Default**: No cross-package dependencies are allowed
- **Stdlib**: `scala.*` and `java.*` packages are always allowed
- **Same package**: Types within the same package are always allowed
- **Subpackages**: A package can depend on its own subpackages
- **Cycles**: The plugin detects and rejects cyclic file dependencies

### Zinc incremental recompilation

When you change the `@dependsOn` annotation on a layer object, Zinc (sbt/mill incremental compiler) must recompile all files in that package. The plugin ensures this by:

1. Adding a synthetic `val hash_<hex> = 1` to each layer object, where the hash is derived from the `@dependsOn` content. When you change dependencies, the hash changes and the layer object is recompiled.
2. Adding a synthetic `private val _layerRef = layer` to the first class/object in each package file (except the layer object file). This creates a dependency so Zinc recompiles those files when the layer object changes.
3. Adding synthetic `private val _layerRef_<pkg> = <pkg>.layer` for each dependent package the class uses. This ensures Zinc recompiles when any of those layer objects change, avoiding stale layer config during incremental compilation.

## Example

A typical layered structure:

```
domain/           # Core business entities (no dependencies)
  layer.scala     # No @dependsOn needed if domain has no cross-package deps
  User.scala

application/      # Use cases / application services
  layer.scala     # @dependsOn("domain")
  UserService.scala

infrastructure/   # External adapters, repositories
  layer.scala     # @dependsOn("domain", "application")
  UserRepository.scala
```

## Building

This project uses [Mill](https://mill-build.com/).

```bash
# Compile
./mill layers.plugin.compile

# Run tests
./mill layers.plugin.test

# Build the plugin JAR
./mill layers.plugin.assembly
```
