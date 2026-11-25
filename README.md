# Layers Plugin

A Scala 3 compiler plugin that enforces layered/onion architecture at compile time by validating package dependencies via `@dependsOnPackages` and `@dependsOnLayers` annotations.

## Overview

The plugin ensures that your codebase respects architectural boundaries. By default, no cross-package dependencies are allowed. Each package must declare its allowed dependencies explicitly using a `layer` object annotated with `@dependsOnPackages` and/or `@dependsOnLayers`.

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

The object must be named `layer`.

Use `@dependsOnPackages` to declare dependencies by package name:

```scala
package application

@layers.dependsOnPackages("domain")
object layer
```

This allows the `application` package to depend on `domain` and any subpackage (e.g. `domain.user`).

### 2. Declare multiple dependencies

```scala
package infrastructure

@layers.dependsOnPackages("domain", "application")
object layer
```

You can also reference another package layer object directly with `@dependsOnLayers`:

```scala
package application

@layers.dependsOnLayers(domain.layer)
object layer
```

### Options

- **maxLayers**: Limit the maximum number of layers allowed (e.g. `-P:layers:maxLayers=5`). Compilation fails if the application has more layers than the limit. The layer count is the **height of the dependency tree** (longest path from root to leaf). Only packages with layer objects are counted; external dependencies (e.g. `scala.*`, `java.*`, third-party libraries) are ignored.

- **design**: Also run checks inferred from the code, on top of the annotation checks (`-P:layers:design`). Annotations can add constraints but never allow what these checks reject. Checks:
  - cyclic dependencies between packages, with the cheapest edges to cut
  - Stable Dependencies Principle: a package may not depend on a less stable one (instability I = Ce / (Ca + Ce))
  - method size and complexity: `maxCyclomatic=10`, `maxCognitive=15`, `maxMethodLines=60` (logical lines: code characters outside comments at 30 per line, so reformatting or golfing into long lines changes nothing), `maxParams=5` (explicit parameters), `maxNesting=4`. A method (or `val`) called from exactly one other method of the same package is a single-use helper and is measured with its caller, whatever its visibility and wherever in the package it is declared (private, public, another file, another class or object): cutting a method into single-use pieces, widening visibility or moving the pieces to another file does not make it pass, the complexity only moves. A single-caller method in a different package is never absorbed: a call across a package boundary is a layer or module boundary, not a split of one method, and that coupling is covered by the package-level checks. A chain of single-use methods adds up at its head, except that a helper which is already over a limit with its own helpers is reported by itself and not counted again in its callers (otherwise every method above it, up to `main`, would repeat the finding). Calls repeated inside one method count as one caller; methods that override an inherited member are never helpers (they are also called through their parent); methods only calling each other with no outside caller stay measured. A method that calls more than `maxHelpers=8` single-use methods is a dispatcher (a router, a composition root, a page assembled from sections), not a split method: its slices are independent and are measured each on its own, not summed. A method and the single-use helpers it absorbed form a cluster, and a cluster is acceptable up to `maxClusterLines=300` logical lines and `maxClusterCyclomatic=30` summed paths when every unit in it (the method and each helper) individually passes all the per-method limits (lines or its own flat allowance, cyclomatic, cognitive, nesting, params): pieces that are each fine are fine, so a page assembled from a dozen cohesive sections, or a route table, is not a finding, while the cap stops unbounded accumulation. A cluster with a unit that is over a limit is not saved by the cap: that unit is reported itself and the rest is summed as before, so cutting a long method into a few long helpers, or into many dense ones (summed paths over the cap), is still caught. Helpers with two or more distinct callers are real reuse and are measured on their own. A finding of a method that absorbed helpers lists what each part contributes, largest first, so it is clear what is actually large. Branching written as method calls costs what it hides: on `Option`, `Either` and `Try`, `fold`, `filterOrElse`, `recover`, `recoverWith` and `getOrElse` with anything but a constant, reference, empty collection or constructor count as one decision like an `if` or a `match` of two cases (turning a `match` into a `fold` does not lower the score); `orElse` chains count like a `||` chain; `Option.when`, `Option.unless` and `Either.cond` count as an `if`. Predicates (`exists`, `forall`, `filter`) and `map`/`flatMap` count only for the branching inside their lambdas, and the same names on collections are not decisions. A by-name branch that is a reference or a plain call (`getOrElse(notFound("x"))`) adds no nesting level, only branching inside it does. Code outside `def`s is measured too: `val`, `lazy val` and `given` initialisers (as the symbol itself), and the statements of a class or object body, which run in the constructor (as `<Class>.<init>`)
  - slop: duplicated methods (same logic up to local names and literals, `minCloneSize=30` syntax nodes), pass-through methods that forward every parameter, in any order and with at most one extra constant, to another class, single-use parameter objects (a case class with at least `minBundleFields=3` fields that is built in one place and read by one method, and appears nowhere else), and traits with a single implementation in the same package
  - responsibilities: `maxResponsibilities=4`. The methods of a class are grouped by what they work on: the domain types of their signatures (the project's own classes; frameworks and the standard library say nothing about what a method is for), mutable state they share, and calls between them. Types, injected fields and helpers used by more than half of the methods are plumbing and link nothing, so adding a shared accessor does not make a class look cohesive. Groups are found by modularity, not connectivity, so a few types that cross aggregates do not chain everything into one group. The finding lists the groups with their topics: split along them (for a data-access class, one per aggregate). Classes of fewer than 4 methods are not judged
  - files: `maxFileLines=1500`. A file that long holds several modules
  - facades: `maxExports=8`. A class whose members are mostly `export` forwarders owns no behaviour; callers still depend on everything it re-exports. Moving methods behind an `export` does not reduce responsibilities

  - objects are judged like classes: moving methods into an `object` (or top-level definitions, or a package object) does not hide them. Utility objects, named `*Util`, `*Utils`, `*Helper(s)`, `*Common`, `*Misc`, `*Tools` or `*Support`, and any object used from 5 or more other packages, are judged as a grab-bag by the same limit with no allowance: methods that mention no domain type (they work on `String`, `Int`, `BigDecimal`...) are not counted as one harmless remainder but each as its own responsibility, so a bucket of unrelated string and number helpers is a finding. Group them by what they work on and move each group next to its data
  - architecture, judged on the domain packages (inferred, see `domain`): a package is domain when its name does not make it infrastructure, support (`util`, `json`) or identity (`auth`), it depends on no infrastructure package of the project (database, rendering, web, integration kinds, inferred from names such as `database`, `routes`, `rendering`, `stripe`, and from well-known libraries), and it is called `model`/`domain` or at least `domainMinDependents=3` packages depend on it. Findings:
    - `domain-purity`: domain code must not read the clock (`LocalDate.now`, `Instant.now`, `Clock`, `System.currentTimeMillis`, ...), draw random values or ids (`Random`, `UUID.randomUUID`), touch files or the environment, write to the console or logs, sleep, or use database, web, rendering or external-service code, and must not take such things (`Clock`, `Random`, a connection, a logger) as constructor or method parameters. A default argument that reads the clock counts. A domain method that calls a helper outside the domain which reads the clock is reported too (the helper is followed up to three calls deep), so moving the call does not help. Fix: pass the time, id or policy in as a parameter, or return what should happen and let the application layer do it. A deliberate seam is exempted by qualified name with `pureExempt=<name>[,<name>]` (class, object or method prefix), visible in the build. One finding per class, with every site
    - `domain-identity`: domain methods, `using` clauses and classes must not take the acting identity or request context (types named `Request`, `Context`, `Session`, `Principal`, `Authentication`, `Credentials`, `RequestContext`, ..., or from an `auth`/`security` package): who may do it is decided in the application layer, what is valid in the domain. A type that is plain data in the domain (`User`, `Utente`) only counts when the parameter is named like the actor (`user`, `actor`, `caller`, `currentUser`) or is compared with ownership data in the method (`doc.ownerId == u.id`); a domain-defined `ValidationContext` is a domain concept. `identityTypes=<name|qualified prefix>[,...]` replaces the vocabulary
    - `god-type`: a class with at least `godTypeMinFields=12` fields that at least `godTypeMinPackages=3` other packages use, where the packages use different parts of it (average pairwise overlap of the field sets at most `godTypeMaxOverlap=50` percent; calls of the class's own methods count as the fields they work on, so accessor wrappers do not hide it). The message lists the fields each package uses: that is the natural split by use case. Also an entity service: a class named `<Noun>Service`/`Manager`/`Handler`/... after a type of the project, with at least `godServiceMinMethods=10` public operations that span at least three of create/update/delete/query and fall into at least two cohesion groups
    - `scattered-transition`: `x.copy(field = <literal | enum case | None | Some(literal)>)` on a domain case class, with the same field and value, in at least `minTransitionSites=3` methods of at least two classes outside the type and its companion (anemic model: the transition belongs to the type). Fix: add a method to the type
    - `layer-mix`: a method (counting single-use helpers with it) that calls into at least `maxLayersPerMethod=4` of the layers persistence, rendering/view, web/routing, external service and domain, and has at least `layerMixMinCyclomatic=4` (three branches) of its own: it parses a request, decides, writes the database, calls a service and renders HTML in one place, so validation, state change and side effects cannot change or be tested apart. A straight-line orchestrator is not judged. The default of 4 accepts the transaction-script handler that parses a request, writes the database and renders the page (web, persistence, rendering): four or more layers in one branching method is the real smell. A method that belongs to an integration package (a sync or messaging service) is the integration: calls inside its own integration package do not count as an external service, only calls to other integrations do. Only calls count: a type in a signature or a data accessor does not; web means the web framework's request and response API (the project's own route helpers, which build links, do not count); one call into the domain (raising an error, formatting a number) is a helper, not the domain layer; a method in a rendering or persistence package is also in that layer. Package kinds come from names and libraries; override with `layerKinds=<pkg>:<kind>[,...]` (kinds `persistence`, `rendering`, `web`, `integration`, `auth`, `domain`, `support`, `none`)
    - `rule-by-relationship` (off by default, enable with `ruleByRelationship`): a domain class holding a collection of the project's own types that is only counted or asked for membership (`size`, `isEmpty`, `contains`, `exists`, ...) by its own methods and nowhere else, and that is not part of a derived codec. Fix: keep the count or enforce the rule in a query

    `domain=<pkg>[,<pkg>]` names the domain packages (with their subpackages) instead of inferring them. Architecture facts depend on `identityTypes` and `layerKinds`: changing them needs a clean compile, which the plugin asks for.

  - slices (`maxSliceFanOut=6`, `maxSliceFanIn=10`, `maxHubInstability=0.3`, `maxHubTypes=60`, `hubOwnedShare=40`, `shared=<pkg;pkg>`, `entry=<pkg;pkg>`): slices should not couple to unrelated slices. A slice is a package that is not shared code: `util`, `common`, `shared`, `support`, `infra`, `platform`, `kernel`, `lib`, `base`, `config`, `json` and `ids` packages, the ones listed in `shared=` (and their subpackages; separate with `;`, a comma would be split by scalac) and small dependency-free packages that at least 3 packages use are shared; a package and its subpackages are one slice unless the outer one holds at least two subpackages (then it is a namespace, and its own files a slice). `slice-fanout <pkg>`: a slice depending on more than `maxSliceFanOut` other slices, except the composition root: a package that no package depends on (Ca = 0) and that holds an entry point (`@main`, `def main(args)`, `extends App`) or is named in `entry=` has to name the slices it wires, so it is exempt (its methods are still measured; Ca = 0 alone does not exempt, a leaf slice with many dependencies is the smell); the message lists the dependencies cheapest first (references to change, reusing the cheapest-edge data of the cycle check) and says how many must go. `slice-hub-unstable <pkg>` and `slice-hub-grabbag <pkg>`: more than `maxSliceFanIn` slices depend on a package, so it is a shared kernel and must be stable (instability at most `maxHubInstability`) (`slice-hub-unstable`); it is a grab-bag (`slice-hub-grabbag`) when it declares more than `maxHubTypes` top-level types (a floor: a big domain model that the slices use together is not a finding) and at least `hubOwnedShare` percent of them are owned by one slice (used by exactly one other slice) or not used outside the package. A type counts as owned only if it can move there without a package cycle: no type that stays behind may use it, and the packages it needs must not depend on that slice; such types are treated as shared and reported as blocked. The message lists the movable types grouped by owning slice. Like the other package-level findings they are judged, under `since=<rev>`, where the file they point at changed. Moving references behind a wrapper package moves the fan-out to the wrapper, and splitting a hub in arbitrary parts that every slice still uses is not detected as progress (known gap)
  - `shared-owned <type>`: a type in a shared package that at least two classes of at least two slices use, where every one of them lives in or depends on the same slice `X` (and nothing else is common to all): it is `X`'s type, moving it adds no dependency to anyone. Narrow on purpose: `X` must not be a hub and the move must not create a cycle
  - cohesion findings on top-level definitions or extension files (`*$package`, `*extensions`) say that the fix is to put each extension next to the type it extends, or one extensions object per receiver family.
  - indirection without a reason, on top of the single-implementation slop check (same package only): `interface-tax <type>`, a trait or abstract class with exactly one implementation in another package and exactly one class using it, where the user could depend on the implementation without creating a package cycle (an interface that breaks a real cycle, or that two classes use, is a boundary). `forwarder-class <class>`, a class (at least 2 methods) all of whose methods are pass-throughs to the same class and that has exactly one caller class. A second implementation or a second user added only to pass is not detected (known gap)

  Findings of one declaration (cyclomatic, cognitive, lines, layer-mix, ...) are each reported at their own source position, so the console shows all of them. Errors list every reference site and fix options. Facts are cached under `META-INF/layers-facts` in the class output so incremental compilation still sees the whole project.
- **baseline=<file>**: accepted legacy findings with the worst accepted value for each (use an absolute path under Mill). New findings, or findings worse than the baseline value, fail. Entries that improved or were fixed also fail until locked in, so improvements cannot regress. Create the file by compiling once with `-P:layers:updateBaseline`. Afterwards `updateBaseline` only lowers or removes entries; adding or raising one is a manual edit.
- **since=<rev>**: ratchet. Per-method limits, slop and class findings fail only where the code changed since `<rev>` (committed, uncommitted or untracked; git must be available), so legacy code is cleaned when someone works on it instead of in a big-bang refactor that rewrites everything to satisfy a metric. A method counts as changed when any line of it, or of a single-use helper counted with it, changed; class, file and facade findings when their declaration line changed or the file is new. Package cycles and stability violations always fail. Everything left alone is summarised in one warning. Ignores `baseline`, which is not needed (use one or the other).
- **history=<window>** (with `minCoChange=40`, `minCoChangeCommits=8`, `minCoChangeLift=1.5`, `minPairCommits=4`, `minPairShare=25`, `wiringPackages=10`, `maxCommitFiles=30`, `maxCommitPackages=6`, `minFeatureLayers=3`, `featureStopWords=word;word`): things that change together belong together. Enables the checks that read git history; `<window>` is `500 commits` (stable: it does not move with the calendar, recommended) or a date such as `1 year ago`. Without `history=` they are off; with git missing, a shallow clone or fewer than 20 usable commits they are skipped with a warning and never fail, and baseline entries of their kinds are left alone. History is read with rename detection and a moved or renamed file keeps its history, so moving files cannot hide a co-change; a file moved and rewritten in one commit (git sees no rename) keeps it too when exactly one current file has its name. Commits touching more than `maxCommitFiles` files (mass renames, reformatting) or more than `maxCommitPackages` packages (adding a plugin, a rename of a shared concept) are cross-cutting maintenance and are ignored; the number of commits used and ignored is printed. Findings: `co-change <a> <-> <b>`: two packages that are touched in the same commit in at least `minCoChange` percent of the commits of the less changed one, in at least `minCoChangeCommits` commits and at least `minCoChangeLift` times more often than chance (a package touched by most commits co-changes with everything: the base rate is not coupling); a package and its sole consumer, and a package and its own subpackage, are not reported; the coupling must be concentrated: at least one file pair changes together in `minPairCommits` commits and accounts for at least `minPairShare` percent of the commits that touch both packages, and neither file is wiring (a file that changed together with files of at least `wiringPackages` other packages is the aggregator every feature touches, and its pairs do not count), otherwise the pair is diffuse (every feature touching a shared wiring or persistence file) and is only named in the note, as that is a hub and fan-in matter and not a capability split over two packages; the message lists the files that changed together most. `feature-spread <noun>`: a business word (singular and plural meet, technical words such as Service, Repository, Page or Form are ignored, add yours with `featureStopWords`) in the file or top-level type names of at least `minFeatureLayers` packages whose files are changed together in at least `minCoChange` percent of the commits that touch them; the message proposes a vertical slice package (`<common prefix>.<noun>`) with the types in those files plus the types only they use, and warns if moving them as they are would create a package cycle. Under `since=<rev>` these findings are enforced when the file they point at changed
- **hotspots** (with optional **history=\"1 year ago\"**): prints the files where complexity and churn multiply, which is where cleaning pays back first, and files in different packages that keep changing together (hidden coupling: a feature spread across layers).
- **report=<file>**: write every measured value, not only the ones over a limit, so it is visible how close each method and class is to its limits. Tab-separated, sorted, one line per value: `kind symbol value limit location detail` with a `#` header. Method kinds `cyclomatic`, `cognitive`, `lines`, `nesting` (measured together with absorbed single-use helpers; `detail` gives the method's own value and what each helper adds, `flat` marks a method under the flat-layout line limit) and `params`; class kinds `responsibilities` (`grab-bag` in `detail` for a utility or high fan-in class), `methods`, `exports`, `fanin` (other packages using an object); file kind `filelines`. Architecture values: `layer-mix` (layers a method touches, for methods of two or more), `slice-fanout`, `slice-fanin`, `slice-instability` (percent), `slice-types` and `slice-owned-share` (percent of types owned by one slice or unused) per slice, `god-fields` and `god-packages` per domain class; every other check (domain purity and identity, scattered transitions, co-change, feature spread, shared-owned, interface-tax...) contributes one `finding` row per finding, with limit `-`. A limit of `-` means none. Written on every compile with `design`, whatever `baseline` and `since` say.
- **metrics**: Print per-package Ca, Ce, instability, abstractness, distance from the main sequence and inferred layer.

### Rules

- **`@dependsOnPackages`/`@dependsOnLayers` placement**: These annotations may only be placed on `object layer`. They fail if placed on a class, trait, or an object with a different name.
- **Default**: No cross-package dependencies are allowed
- **Stdlib**: `scala.*` and `java.*` packages are always allowed
- **Same package**: Types within the same package are always allowed
- **Subpackages**: A package can depend on its own subpackages
- **Cycles**: The plugin detects and rejects cyclic file dependencies

### Zinc incremental recompilation

When you change `@dependsOnPackages`/`@dependsOnLayers` on a layer object, Zinc (sbt/mill incremental compiler) must recompile all files in that package. The plugin ensures this by:

1. Adding a synthetic `val hash_<hex> = 1` to each layer object, where the hash is derived from layer dependency annotation content. When you change dependencies, the hash changes and the layer object is recompiled.
2. Adding a synthetic `private val _layerRef = layer` to the first class/object in each package file (except the layer object file). This creates a dependency so Zinc recompiles those files when the layer object changes.
3. Adding synthetic `private val _layerRef_<pkg> = <pkg>.layer` for each dependent package the class uses. This ensures Zinc recompiles when any of those layer objects change, avoiding stale layer config during incremental compilation.

## Example

A typical layered structure:

```
domain/           # Core business entities (no dependencies)
  layer.scala     # No annotations needed if domain has no cross-package deps
  User.scala

application/      # Use cases / application services
  layer.scala     # @dependsOnPackages("domain")
  UserService.scala

infrastructure/   # External adapters, repositories
  layer.scala     # @dependsOnPackages("domain", "application")
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
