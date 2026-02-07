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

- **maxLayers**: Limit the maximum number of layers allowed (e.g. `-P:layers:maxLayers=5`). Compilation fails if the application has more layers than the limit.

### Rules

- **@dependsOn placement**: The annotation may only be placed on `object layer`. It fails if placed on a class, trait, or an object with a different name.
- **Default**: No cross-package dependencies are allowed
- **Stdlib**: `scala.*` and `java.*` packages are always allowed
- **Same package**: Types within the same package are always allowed
- **Subpackages**: A package can depend on its own subpackages
- **Cycles**: The plugin detects and rejects cyclic file dependencies

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
