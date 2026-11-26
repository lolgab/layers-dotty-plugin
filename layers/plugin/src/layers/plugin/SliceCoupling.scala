package layers.plugin

/** Options of the slice checks, read from `maxSliceFanOut=6` style plugin options.
  *
  * @param maxFanOut
  *   most other slices a slice may depend on
  * @param maxFanIn
  *   most slices that may depend on one slice before it has to be a small, stable kernel
  * @param maxHubInstability
  *   a package many slices depend on may depend on at most this share of the packages around it (Ce / (Ca + Ce))
  * @param maxHubTypes
  *   a package many slices depend on is judged a grab-bag only when it declares more top-level types than this (a floor)
  * @param hubOwnedShare
  *   ... and at least this percent of its types are owned (used by exactly one other slice, and movable there without a
  *   package cycle) or not used outside the package
  * @param entry
  *   packages that are composition roots: nothing depends on them and they name the slices they wire (they also are
  *   when they hold an entry point)
  * @param shared
  *   packages (and their subpackages) that are infrastructure or shared code, not slices
  */
final case class SliceOptions(
    maxFanOut: Int = 6,
    maxFanIn: Int = 10,
    maxHubInstability: Double = 0.3,
    maxHubTypes: Int = 60,
    shared: List[String] = Nil,
    hubOwnedShare: Int = 40,
    entry: List[String] = Nil
)

object SliceOptions:
  def fromOptions(options: List[String]): SliceOptions =
    def value(name: String) = options.collectFirst { case o if o.startsWith(name + "=") => o.drop(name.length + 1).trim }
    def int(name: String, default: Int) = value(name).flatMap(_.toIntOption).filter(_ > 0).getOrElse(default)
    val d = SliceOptions()
    SliceOptions(
      int("maxSliceFanOut", d.maxFanOut),
      int("maxSliceFanIn", d.maxFanIn),
      value("maxHubInstability").flatMap(_.toDoubleOption).filter(x => x > 0 && x <= 1).getOrElse(d.maxHubInstability),
      int("maxHubTypes", d.maxHubTypes),
      value("shared").map(_.split("[,;]").toList.map(_.trim).filter(_.nonEmpty)).getOrElse(Nil),
      int("hubOwnedShare", d.hubOwnedShare),
      value("entry").map(_.split("[,;]").toList.map(_.trim).filter(_.nonEmpty)).getOrElse(Nil)
    )

/** Which packages are vertical slices (a capability of the business) and which are shared infrastructure.
  *
  * A package is shared when its name says so (`util`, `common`, `shared`, `support`, `infra`, `platform`, `kernel`,
  * `lib`, `base`, `config`, `json`, `ids`), when it is listed in `shared=`, or when it is a small foundation: it depends
  * on nothing of the project, at least `foundationFanIn` packages use it and it declares at most `maxHubTypes` types (a
  * big package with no dependencies is a grab-bag, which is judged as a slice). Everything else is a slice.
  */
final class Slices(val graph: PackageGraph, val facts: List[FileFacts], val options: SliceOptions):
  private val sharedNames =
    Set("util", "utils", "common", "shared", "support", "infra", "infrastructure", "platform", "kernel", "lib", "base", "config", "json", "ids")
  private val foundationFanIn = 3
  private val projectPackages = graph.nodes.toSet
  private val typeCount: Map[String, Int] =
    facts.flatMap(_.typeDecls).filter(d => !d.local && Slices.isTopLevel(d)).map(d => d.pkg -> d.sym).distinct.groupBy(_._1).view.mapValues(_.size).toMap

  private val entryPackages = facts.flatMap(_.arch).filter(_.kind == "entry").map(_.field(0)).toSet

  /** The application root: no package depends on it and it holds the entry point (`@main`, `def main`, `extends App`) or
    * is named in `entry=`. It has to name the slices it wires, so many dependencies are its job; its methods are still
    * measured. A package nothing depends on that holds no entry point is just a leaf slice and is judged.
    */
  def compositionRoot(pkg: String): Boolean =
    graph.pred.getOrElse(pkg, Nil).isEmpty &&
      (entryPackages(pkg) || options.entry.exists(e => pkg == e))

  def isShared(pkg: String): Boolean =
    pkg.split('.').exists(sharedNames) ||
      options.shared.exists(s => pkg == s || pkg.startsWith(s + ".")) ||
      (graph.efferent(pkg) == 0 && graph.afferent(pkg) >= foundationFanIn && typeCount.getOrElse(pkg, 0) <= options.maxHubTypes)

  val slices: List[String] = graph.nodes.filter(isSlice)

  private val family = Slices.families(graph.nodes)

  /** A package and its subpackages are one slice family: depending on a subpackage does not couple two slices. A
    * package that merely holds several subpackages (`app` over `app.sian`, `app.catasto`) is a namespace, not a family.
    */
  def related(a: String, b: String): Boolean = family(a, b)

  /** A package of the project that is not shared code; external packages are neither. */
  def isSlice(pkg: String): Boolean = projectPackages(pkg) && !isShared(pkg)

  /** The other slices this one depends on. */
  def dependencies(pkg: String): List[String] =
    graph.succ.getOrElse(pkg, Nil).filter(p => isSlice(p) && !related(pkg, p))

  /** The other slices depending on this one. */
  def dependents(pkg: String): List[String] =
    graph.pred.getOrElse(pkg, Nil).filter(p => isSlice(p) && !related(pkg, p))

  def instability(pkg: String): Double =
    val (ca, ce) = (graph.afferent(pkg), graph.efferent(pkg))
    if ca + ce == 0 then 0.0 else ce.toDouble / (ca + ce)

  /** Top-level classes, traits and objects declared in a package. */
  def topLevelTypes(pkg: String): List[TypeDecl] =
    facts.flatMap(_.typeDecls).filter(d => d.pkg == pkg && !d.local && Slices.isTopLevel(d)).distinctBy(_.sym).sortBy(_.sym)

object Slices:
  /** `related(a, b)`: the same package, or one inside the other, unless the outer one is a namespace holding at least two
    * subpackages (then its subpackages are slices of their own, and its own files a slice too).
    */
  def families(nodes: List[String]): (String, String) => Boolean =
    val all = nodes.toSet
    def children(p: String) = all.count(q => q.startsWith(p + ".") && !q.drop(p.length + 1).contains('.'))
    val namespaces = nodes.filter(children(_) >= 2).toSet
    (a, b) =>
      a == b || (b.startsWith(a + ".") && !namespaces(a)) || (a.startsWith(b + ".") && !namespaces(b))

  /** The type is declared directly in its package, not inside another class. */
  def isTopLevel(d: TypeDecl): Boolean =
    val rest = d.sym.stripPrefix(d.pkg + ".")
    rest.nonEmpty && !rest.contains('.')

  /** The top-level class containing a declaration. */
  def topOf(d: TypeDecl): String = d.pkg + "." + d.sym.stripPrefix(d.pkg + ".").takeWhile(_ != '.')

  def simple(sym: String): String = sym.split('.').last

  /** Two decimals, always with a point: messages must not depend on the machine's locale. */
  def dec(x: Double): String = String.format(java.util.Locale.ROOT, "%.2f", Double.box(x))

/** Slices should not couple to unrelated slices (Derek Comartin): a slice that depends on many others cannot change
  * or be understood on its own, and a slice that many others depend on is a shared kernel, which must be small and
  * stable, not a grab-bag every change touches.
  *
  * Findings:
  *   - `slice-fanout <pkg>`: depends on more than `maxSliceFanOut` other slices, with the cheapest dependencies to cut
  *   - `slice-hub-unstable <pkg>`: more than `maxSliceFanIn` slices depend on it, and it depends on too much itself
  *   - `slice-hub-grabbag <pkg>`: more than `maxSliceFanIn` slices depend on it, and it declares more than
  *     `maxHubTypes` top-level types
  */
object SliceCoupling:

  private val maxSitesShown = 3
  private val maxEdgesShown = 12

  /** Whole file: under the `since` ratchet the finding is enforced when someone touches the file it points at. */
  private val wholeFile = List((1, Int.MaxValue))

  private def edge(graph: PackageGraph, from: String, to: String): PackageCycles.CutEdge =
    PackageCycles.CutEdge(from, to, graph.sites((from, to)).distinctBy(e => (e.file, e.line)))

  def findings(slices: Slices): List[Finding] =
    val fanOut = slices.slices.flatMap(p => fanOutFinding(slices, p))
    val hubs = slices.slices.flatMap(p => hubFindings(slices, p))
    (fanOut ++ hubs).sortBy(f => (f.file, f.line, f.key))

  private def fanOutFinding(s: Slices, pkg: String): Option[Finding] =
    val deps = s.dependencies(pkg)
    val limit = s.options.maxFanOut
    if deps.size <= limit || s.compositionRoot(pkg) then None
    else
      val edges = deps.map(edge(s.graph, pkg, _)).sortBy(e => (e.sites.size, e.to))
      val excess = deps.size - limit
      val cheapest = edges.take(math.min(maxEdgesShown, excess + 2))
      val sb = new StringBuilder
      sb.append(s"Slice `$pkg` depends on ${deps.size} other slices (limit $limit): ${deps.sorted.mkString(", ")}.\n")
      sb.append(
        "  Principle: slices should not couple to unrelated slices. A slice that needs this many others cannot be " +
          "changed, tested or understood on its own, and every one of them is a reason for it to change.\n"
      )
      sb.append(s"  Cut at least $excess of these dependencies. Cheapest first (references to change):\n")
      for e <- cheapest do
        sb.append(s"    $pkg -> ${e.to} (${e.sites.size} reference${if e.sites.size == 1 then "" else "s"}):\n")
        for site <- e.sites.take(maxSitesShown) do sb.append(s"      ${site.file}:${site.line}  ${site.fromSym} -> ${site.toSym}\n")
        if e.sites.size > maxSitesShown then sb.append(s"      ... and ${e.sites.size - maxSitesShown} more\n")
      sb.append("  Fix each dependency `A -> B` with one of:\n")
      sb.append("    - Move the code that uses B into B's slice (or into the slice that owns the use case), so A stops knowing B.\n")
      sb.append("    - Let A publish what happened (a callback, an event, a trait declared in A) and have B react, instead of A calling B.\n")
      sb.append("    - If A only orchestrates, make that orchestration a slice of its own and keep A free of it.\n")
      sb.append("  Moving the references behind a wrapper, `export` or a shared object does not reduce what A depends on.\n")
      sb.append(s"  Done when `$pkg` depends on at most $limit other slices.")
      val site = cheapest.headOption.flatMap(_.sites.headOption)
      Some(
        Finding(s"slice-fanout $pkg", deps.size, site.map(_.file).getOrElse(""), site.map(_.line).getOrElse(1), sb.toString, wholeFile)
      )

  private def hubFindings(s: Slices, pkg: String): List[Finding] =
    val users = s.dependents(pkg)
    val o = s.options
    if users.size <= o.maxFanIn then Nil
    else
      val types = s.topLevelTypes(pkg)
      val instability = s.instability(pkg)
      val site = s.graph.pred.getOrElse(pkg, Nil).headOption.flatMap(p => s.graph.sites.get((p, pkg))).flatMap(_.headOption)
      val file = site.map(_.file).getOrElse("")
      val line = site.map(_.line).getOrElse(1)
      // two findings of one package must not share a position: the compiler shows only one error per position
      val first = types.headOption
      val unstable = Option.when(instability > o.maxHubInstability + 1e-9) {
        val deps = s.graph.succ.getOrElse(pkg, Nil)
        val text =
          s"Package `$pkg` is a hub: ${users.size} slices depend on it (limit ${o.maxFanIn}), yet it is unstable " +
            s"(instability ${Slices.dec(instability)}, limit ${Slices.dec(o.maxHubInstability)}): it depends on ${deps.mkString(", ")}.\n" +
            "  Principle: a package that many slices depend on is a shared kernel. It changes for every one of them, so it " +
            "must depend on almost nothing, or each of its dependencies is a reason for all its dependents to change.\n" +
            "  Fix with one of:\n" +
            s"    - Remove the dependencies of `$pkg` (invert them with a trait declared in `$pkg`, or move the code that needs them out).\n" +
            "    - Stop sharing: move what only some slices use into those slices, so fewer slices depend on the package.\n" +
            s"  Done when `$pkg` is used by at most ${o.maxFanIn} slices or its instability is at most ${o.maxHubInstability}."
        Finding(s"slice-hub-unstable $pkg", math.round(instability * 100).toInt, file, line, text, wholeFile)
      }
      val own = hubOwnership(s, pkg, types)
      val grabBag = Option.when(types.size > o.maxHubTypes && own.percent >= o.hubOwnedShare) {
        val sb = new StringBuilder
        sb.append(
          s"Package `$pkg` is a hub and a grab-bag: ${users.size} slices depend on it (limit ${o.maxFanIn}), it declares " +
            s"${types.size} top-level types (more than ${o.maxHubTypes}) and ${own.percent}% of them (limit ${o.hubOwnedShare}%) " +
            "belong to a single slice or to nobody else.\n"
        )
        sb.append(
          "  Principle: cohesion by business capability. A shared kernel is what several slices use together; types that " +
            "one slice alone uses are that slice's own, and in the kernel they make every feature change the package " +
            "everything depends on.\n"
        )
        sb.append(s"  ${own.owned.values.map(_.size).sum} of the ${types.size} types are used by one slice only and can move there without a package cycle:\n")
        for (owner, names) <- own.owned.toList.sortBy(x => (-x._2.size, x._1)).take(8) do
          val shown = names.map(Slices.simple).sorted
          sb.append(s"    move to `$owner` (${shown.size}): ${shown.take(8).mkString(", ")}${if shown.size > 8 then ", ..." else ""}\n")
        if own.unused > 0 then sb.append(s"  ${own.unused} types are not used by any other slice (used inside `$pkg` only or not at all).\n")
        if own.blocked > 0 then
          sb.append(s"  ${own.blocked} more types have a single user slice but cannot move alone (other types of `$pkg` use them, or they need a package that depends on that slice); they count as shared.\n")
        sb.append("  Fix with one of:\n")
        sb.append("    - Move each type that one slice uses into that slice, as listed above.\n")
        sb.append("    - Group the types used together by the same slices into packages named after the capability they serve.\n")
        sb.append(s"  Splitting `$pkg` into arbitrary parts does not count: the dependents still depend on all of them.\n")
        sb.append(s"  Done when `$pkg` declares at most ${o.maxHubTypes} top-level types, fewer than ${o.hubOwnedShare}% of them are owned by one slice or unused, or at most ${o.maxFanIn} slices depend on it.")
        Finding(s"slice-hub-grabbag $pkg", types.size, first.map(_.file).getOrElse(file), first.map(_.line).getOrElse(line), sb.toString, wholeFile)
      }
      unstable.toList ++ grabBag.toList

  /** Which types of a package are owned by one slice.
    *
    * @param owned
    *   owning slice -> the types only it uses that can move there
    * @param unused
    *   types no other slice uses
    * @param blocked
    *   types with one user slice that cannot move alone
    */
  private[plugin] final case class Ownership(total: Int, owned: Map[String, List[String]], unused: Int, blocked: Int):
    def percent: Int = if total == 0 then 0 else ((owned.values.map(_.size).sum + unused) * 100.0 / total).round.toInt

  /** A type with one user slice `X` is `X`'s only if moving it adds no cycle: no other type of the package that stays
    * behind uses it (the package would then depend on `X`, which depends on the package), and none of the packages it
    * needs reaches `X` (X would depend on them, and they on X). Types that move together are judged together.
    */
  private[plugin] def hubOwnership(s: Slices, pkg: String, types: List[TypeDecl]): Ownership =
    val tops = types.map(Slices.topOf).distinct
    val topSet = tops.toSet
    val outside = hubUsage(s, pkg, types).toMap
    val deps = s.facts.flatMap(_.classDeps)
    val inside = deps.filter(d => d.toPkg == pkg && d.fromPkg == pkg && topSet(d.toTop) && d.fromTop != d.toTop)
      .groupBy(_.toTop).view.mapValues(_.map(_.fromTop).toSet).toMap
    val needs = deps.filter(d => d.fromPkg == pkg && topSet(d.fromTop) && d.toPkg != pkg)
      .groupBy(_.fromTop).view.mapValues(_.map(_.toPkg).toSet).toMap
    def reaches(from: String, to: String): Boolean =
      @annotation.tailrec def go(todo: List[String], seen: Set[String]): Boolean = todo match
        case Nil => false
        case x :: rest => if x == to then true else if seen(x) then go(rest, seen) else go(s.graph.succ.getOrElse(x, Nil) ++ rest, seen + x)
      go(List(from), Set.empty)
    val single = tops.filter(t => outside.getOrElse(t, Nil).size == 1)
    val byOwner = single.groupBy(t => outside(t).head)
    val movable = byOwner.map { case (owner, ts) =>
      var set = ts.filter(t => needs.getOrElse(t, Set.empty).forall(d => d == owner || !reaches(d, owner))).toSet
      var changed = true
      while changed do
        val keep = set.filter(t => inside.getOrElse(t, Set.empty).forall(set))
        changed = keep.size != set.size
        set = keep
      owner -> ts.filter(set).sorted
    }.filter(_._2.nonEmpty)
    val unused = tops.count(t => outside.getOrElse(t, Nil).isEmpty)
    Ownership(tops.size, movable, unused, single.size - movable.values.map(_.size).sum)

  /** For each top-level type of the package, the other slices that use it. */
  private def hubUsage(s: Slices, pkg: String, types: List[TypeDecl]): List[(String, List[String])] =
    val users = s.facts.flatMap(_.classDeps).filter(d => d.toPkg == pkg && s.isSlice(d.fromPkg) && !s.related(pkg, d.fromPkg))
      .groupBy(_.toTop).view.mapValues(_.map(_.fromPkg).distinct.sorted).toMap
    types.map(t => Slices.topOf(t)).distinct.map(top => top -> users.getOrElse(top, Nil))

/** The checks that work on the dependency data alone, and not on history. */
object SliceChecks:
  def findings(facts: List[FileFacts], graph: PackageGraph, options: List[String]): List[Finding] =
    val slices = new Slices(graph, facts, SliceOptions.fromOptions(options))
    SliceCoupling.findings(slices) ++ SharedOwnership.findings(slices) ++ InterfaceTax.findings(facts, graph)
