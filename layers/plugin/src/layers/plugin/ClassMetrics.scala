package layers.plugin

import scala.collection.mutable

/** Limits for method and class measurements. A measurement over its limit is a finding. */
final case class Thresholds(
    maxCyclomatic: Int = 10,
    maxCognitive: Int = 15,
    maxMethodLines: Int = 60,
    maxFlatLines: Int = 150,
    maxParams: Int = 5,
    maxNesting: Int = 4,
    maxResponsibilities: Int = 4,
    maxExports: Int = 8,
    minCloneSize: Int = 30,
    minBundleFields: Int = 3,
    maxFileLines: Int = 1500,
    maxHelpers: Int = 8,
    maxClusterLines: Int = 300,
    maxClusterCyclomatic: Int = 30
)

object Thresholds:
  /** Reads `maxCyclomatic=10` style options; unknown or malformed ones are ignored. */
  def fromOptions(options: List[String]): Thresholds =
    def get(name: String, default: Int): Int =
      options.collectFirst { case o if o.startsWith(name + "=") => o.drop(name.length + 1).trim.toIntOption }.flatten
        .filter(_ > 0).getOrElse(default)
    val d = Thresholds()
    Thresholds(
      get("maxCyclomatic", d.maxCyclomatic),
      get("maxCognitive", d.maxCognitive),
      get("maxMethodLines", d.maxMethodLines),
      get("maxFlatLines", d.maxFlatLines),
      get("maxParams", d.maxParams),
      get("maxNesting", d.maxNesting),
      get("maxResponsibilities", d.maxResponsibilities),
      get("maxExports", d.maxExports),
      get("minCloneSize", d.minCloneSize),
      get("minBundleFields", d.minBundleFields),
      get("maxFileLines", d.maxFileLines),
      get("maxHelpers", d.maxHelpers),
      get("maxClusterLines", d.maxClusterLines),
      get("maxClusterCyclomatic", d.maxClusterCyclomatic)
    )

/** Turns method and class measurements into findings with an explanation and a fix. */
object ClassMetrics:

  private def method(
      m: MethodFact, kind: String, value: Int, limit: Int, what: String, principle: String, fixes: List[String],
      note: String = "",
      ranges: List[(Int, Int)] = Nil,
      breakdown: String = ""
  ) =
    val text = new StringBuilder
    text.append(s"Method `${m.sym}` $what: $value (limit $limit)$note.\n")
    text.append(breakdown)
    text.append(s"  Principle: $principle\n")
    text.append(s"  Fix with one of:\n")
    for f <- fixes do text.append(s"    - $f\n")
    text.append(s"  Done when `${m.sym}` measures $limit or less.")
    Finding(s"$kind ${m.sym}", value, m.file, m.line, text.toString, if ranges.isEmpty then List((m.line, m.line + math.max(m.lines, 1) - 1)) else ranges)

  private val splitSteps = "Extract each step or branch body into a private method named after what it does."

  /** A method's cost seen by a reader: its own, plus that of the helpers only it uses. Cutting a method into
    * single-use pieces moves the complexity, it does not remove it, so the pieces are measured with the caller.
    *
    * @param own
    *   the method alone, without what was absorbed
    * @param parts
    *   the absorbed helpers alone, each with its own measurements
    * @param cluster
    *   the method and its helpers are each within every per-method limit and together within the cluster caps
    *   (`maxClusterLines`, `maxClusterCyclomatic`): pieces that are each fine, assembled, are fine, so the sums are not
    *   judged
    */
  private[plugin] final case class Effective(
      fact: MethodFact, absorbed: List[String], ranges: List[(Int, Int)], own: MethodFact, parts: List[MethodFact],
      cluster: Boolean = false
  )

  private[plugin] def effective(methods: List[MethodFact], t: Thresholds = Thresholds()): List[Effective] =
    val merged = methods.groupBy(_.sym).values.map { ms =>
      ms.reduce { (a, b) =>
        a.copy(
          cyclomatic = math.max(a.cyclomatic, b.cyclomatic), cognitive = math.max(a.cognitive, b.cognitive),
          lines = math.max(a.lines, b.lines), size = math.max(a.size, b.size), params = math.max(a.params, b.params),
          nesting = math.max(a.nesting, b.nesting), priv = a.priv && b.priv, calls = (a.calls ++ b.calls).distinct,
          entry = a.entry || b.entry
        )
      }
    }.toList
    val bySym = merged.map(m => m.sym -> m).toMap
    val callers = merged.flatMap(m => m.calls.filter(bySym.contains).map(_ -> m.sym)).groupMap(_._1)(_._2).view.mapValues(_.distinct).toMap
    // A helper is a method exactly one other method of the same package uses, whatever its visibility and whichever
    // file, class or object it is in: making it public or moving it elsewhere in the package is not a design change,
    // the code still serves one caller. A call across a package boundary is a layer or module boundary, not a split
    // of one method (package-level checks cover that coupling), so such a method is measured on its own. A
    // method that overrides an inherited one is also called through its parent, so it is never a helper. A call
    // repeated within one method counts once, and two methods calling each other with no outside caller are roots.
    def candidate(sym: String): Boolean =
      !bySym(sym).entry && callers.get(sym).exists(cs => cs.size == 1 && cs.head != sym && bySym(cs.head).pkg == bySym(sym).pkg)
    def helper(sym: String): Boolean =
      @annotation.tailrec def loop(s: String, seen: Set[String]): Boolean =
        if seen(s) then false
        else if !candidate(s) then true
        else loop(callers(s).head, seen + s)
      candidate(sym) && loop(callers(sym).head, Set(sym))
    // A helper whose own total is already over a limit is a finding by itself: counting it again in its caller, and in
    // the caller's caller up to `main`, would only bury it. Pieces that each stay within the limits are what splitting
    // a method produces, and those add up at their head.
    def over(f: MethodFact): Boolean =
      f.cyclomatic > t.maxCyclomatic || f.cognitive > t.maxCognitive || f.size > limitFor(f, t) || f.nesting > t.maxNesting
    // A cluster (a method and the single-use helpers of its package it absorbed) is acceptable when every unit passes all
    // the per-method limits on its own and the sum stays within the caps: a page of twenty fine sections is fine, the
    // caps stop unbounded accumulation, and cutting a long method into pieces that are each too long still fails.
    def acceptable(sym: String, names: List[String], f: MethodFact): Boolean =
      val units = (sym :: names).distinct.map(bySym)
      names.nonEmpty && units.forall(u => !over(u) && u.params <= t.maxParams) &&
      f.size <= t.maxClusterLines && f.cyclomatic <= t.maxClusterCyclomatic
    def exceeds(sym: String, f: MethodFact, names: List[String]): Boolean = over(f) && !acceptable(sym, names, f)
    val memo = mutable.Map[String, (MethodFact, List[String])]()
    val active = mutable.Set[String]()
    def total(sym: String): (MethodFact, List[String]) =
      memo.get(sym) match
        case Some(r) => r
        case None if active(sym) => (bySym(sym), Nil)
        case None =>
          active += sym
          val m = bySym(sym)
          val direct = m.calls.distinct.filter(c => bySym.contains(c) && c != sym && helper(c) && !active(c))
          // A method calling more single-use methods than a split ever produces is a dispatcher (a router, a composition
          // root, a page assembled from sections): independent slices, each measured on its own, not summed.
          val inner = (if direct.size > t.maxHelpers then Nil else direct).flatMap { c =>
            val (f, names) = total(c)
            if exceeds(c, f, names) then None else Some((f, c :: names))
          }
          val fact = inner.foldLeft(m) { case (acc, (f, _)) =>
            acc.copy(
              cyclomatic = acc.cyclomatic + f.cyclomatic - 1, cognitive = acc.cognitive + f.cognitive,
              lines = acc.lines + f.lines, size = acc.size + f.size, nesting = math.max(acc.nesting, f.nesting)
            )
          }
          active -= sym
          val r = (fact, inner.flatMap(_._2))
          memo(sym) = r
          r
    merged.flatMap { m =>
      val (f, names0) = total(m.sym)
      val names = names0.distinct
      if helper(m.sym) && !exceeds(m.sym, f, names) then None
      else
        val ranges = (m.sym :: names).map(bySym).map(x => (x.line, x.line + math.max(x.lines, 1) - 1))
        Some(Effective(f, names, ranges, m, names.map(bySym), acceptable(m.sym, names, f)))
    }

  private def absorbedNote(e: Effective): String =
    if e.absorbed.isEmpty then ""
    else
      val names = e.absorbed.map(_.split('.').last).distinct
      s" (counting ${names.size} single-use helper${if names.size == 1 then "" else "s"}: ${names.take(5).mkString(", ")}" +
        s"${if names.size > 5 then ", ..." else ""}; splitting a method into pieces only it uses moves the complexity, it does not remove it)"

  /** Where a measurement of a method that absorbed helpers comes from: what each part contributes, largest first, so
    * the reader knows what to look at.
    */
  private def breakdown(e: Effective, what: String, own: MethodFact => Int, helper: MethodFact => Int): String =
    if e.parts.isEmpty then ""
    else
      def where(m: MethodFact) = s"${java.nio.file.Path.of(m.file).getFileName}:${m.line}"
      val rows = (s"${e.own.sym} itself (${where(e.own)})", own(e.own), "") ::
        e.parts.map(p => (s"${p.sym} (${where(p)})", helper(p), "+"))
      val sorted = rows.sortBy(-_._2)
      val shown = sorted.take(8).map { case (n, v, sign) => s"    - $n: $sign$v" }
      s"  Where the $what comes from, largest first (${e.parts.size} absorbed helper${if e.parts.size == 1 then "" else "s"}, " +
        s"each called from nowhere else):\n${shown.mkString("\n")}\n" +
        (if sorted.size > 8 then s"    - ... and ${sorted.size - 8} smaller parts\n" else "")

  /** Length without logic is layout (a page template, a mapping table): reading it is cheap, so it may run longer. */
  private[plugin] def limitFor(m: MethodFact, t: Thresholds): Int =
    if m.cognitive <= t.maxCognitive / 2 && m.cyclomatic <= t.maxCyclomatic - 2 then math.max(t.maxFlatLines, t.maxMethodLines)
    else t.maxMethodLines

  def findings(methods: List[MethodFact], classes: List[ClassFact], t: Thresholds, fanIn: Map[String, Int] = Map.empty): List[Finding] =
    val fromMethods = effective(methods, t).filterNot(_.cluster).flatMap { e =>
      val m = e.fact
      List(
        Option.when(m.cyclomatic > t.maxCyclomatic)(
          method(
            m, "cyclomatic", m.cyclomatic, t.maxCyclomatic, "has too many independent paths (cyclomatic complexity)",
            "A method should be testable with a handful of cases (McCabe); every path needs its own test.",
            List(
              splitSteps,
              "Replace a chain of conditions on a type or tag with polymorphism or a lookup `Map`.",
              "Use guard clauses and pattern matching to remove else branches."
            ),
            absorbedNote(e), e.ranges, breakdown(e, "cyclomatic complexity", _.cyclomatic, x => math.max(x.cyclomatic - 1, 0))
          )
        ),
        Option.when(m.cognitive > t.maxCognitive)(
          method(
            m, "cognitive", m.cognitive, t.maxCognitive, "is too hard to follow (cognitive complexity)",
            "Code should be understandable top to bottom without holding many branches in your head; nesting costs most.",
            List(
              splitSteps,
              "Flatten nesting: handle the exceptional cases first, or use a for-comprehension over Option/Either.",
              "Name complex boolean conditions as `val`s or small predicates."
            ),
            absorbedNote(e), e.ranges, breakdown(e, "cognitive complexity", _.cognitive, _.cognitive)
          )
        ),
        Option.when(m.size > limitFor(m, t))(
          method(
            m, "lines", m.size, limitFor(m, t), "is too long (logical lines: code characters at 30 per line, formatting does not matter)",
            "A function should do one thing (Clean Code, ch. 3); long bodies mix several levels of abstraction.",
            List(splitSteps, "Move groups of statements that work on the same data into their own class."),
            absorbedNote(e), e.ranges, breakdown(e, "size", _.size, _.size)
          )
        ),
        Option.when(m.nesting > t.maxNesting)(
          method(
            m, "nesting", m.nesting, t.maxNesting, "is nested too deeply (nesting depth)",
            "Each nesting level multiplies the context a reader must keep in mind.",
            List(
              "Extract the innermost block into a named method.",
              "Invert conditions into guard clauses or pattern matches to flatten the structure."
            ),
            absorbedNote(e), e.ranges, breakdown(e, "nesting", _.nesting, _.nesting)
          )
        )
      ).flatten
    }
    val fromParams = methods.filter(_.params > t.maxParams).map { m =>
      method(
        m, "params", m.params, t.maxParams, "takes too many parameters (explicit parameters)",
        "Long parameter lists hide a missing concept and are easy to call wrongly (Clean Code, ch. 3).",
        List(
          "Group parameters that travel together into a case class and pass that, if it names a real concept: a bundle " +
            "built in one place and read in one method only hides the list.",
          "Check whether some parameters are state of the receiver and belong in a field or constructor."
        )
      )
    }
    val fromClasses = classes.flatMap(c => cohesion(c, t, grabBag(c, fanIn)) ++ facade(c, t))
    (fromMethods ++ fromParams ++ fromClasses).groupBy(_.key).values.map(_.maxBy(_.value)).toList.sortBy(f => (f.file, f.line, f.key))

  /** A source file too long to hold in mind is a module that was never split, whatever its parts look like. */
  def fileFindings(files: List[FileFacts], t: Thresholds): List[Finding] =
    files.filter(_.lines > t.maxFileLines).map { f =>
      val text =
        s"File `${f.source}` has ${f.lines} lines (limit ${t.maxFileLines}).\n" +
          "  Principle: a file is the unit people open, search and merge. One that long hides several modules in one: " +
          "every change collides with every other, and nobody can tell what it contains without reading it.\n" +
          "  Fix: split it along the responsibilities of what it declares, one file per cohesive group, named after " +
          "what the group is for.\n" +
          s"  Done when `${f.source}` has at most ${t.maxFileLines} lines."
      Finding(s"filesize ${java.nio.file.Path.of(f.source).getFileName}", f.lines, f.source, 1, text)
    }

  /** Methods that work on the same things: they share a domain type, a field or call each other. */
  private[plugin] final case class Responsibility(methods: List[String], types: List[String])

  /** Fraction of a class's methods a type, field or helper must be used by to count as plumbing, not as a topic. */
  private val hubShare = 0.5

  /** Below this many methods there is no telling plumbing from a topic. */
  private val hubMinMethods = 8

  /** Groups the methods of a class by what they work on.
    *
    * Methods are linked by the topics they share: a domain type, a field, a call. A topic shared by few methods is a
    * strong link and one shared by many a weak one. A type, injected dependency or helper that most methods use (a
    * database handle, a request context, a transaction wrapper) says nothing about what a method is for, so it links
    * nothing: otherwise adding one shared accessor would make any class look cohesive. Groups are then found by
    * modularity, not by connectivity, because a few types that cross aggregates would otherwise chain every aggregate
    * into one group.
    */
  private[plugin] def responsibilities(c: ClassFact): List[Responsibility] =
    val ms = c.methods.groupBy(_.name).values.map(g => g.head.copy(
      types = g.flatMap(_.types).distinct, calls = g.flatMap(_.calls).distinct, fields = g.flatMap(_.fields).distinct
    )).toList.sortBy(_.name)
    val n = ms.size
    def hub(count: Int) = n >= hubMinMethods && count.toDouble / n > hubShare
    def uses[A](f: ClassMethod => List[A]) = ms.flatMap(m => f(m).distinct).groupBy(identity).view.mapValues(_.size).toMap
    val typeUse = uses(_.types)
    val fieldUse = uses(_.fields)
    val callUse = uses(_.calls)
    val names = ms.map(_.name).toSet
    // A field that is mutable is state the methods really share; an injected dependency used by most methods is not.
    def tiesFields(f: String) = f.endsWith("!") || !hub(fieldUse(f))
    val helpers = names.filter(x => hub(callUse.getOrElse(x, 0)))
    val kept = ms.filterNot(m => helpers(m.name))
    val index = kept.map(_.name).zipWithIndex.toMap
    val weights = collection.mutable.Map[(Int, Int), Double]().withDefaultValue(0.0)
    def link(a: Int, b: Int, w: Double): Unit =
      if a != b then weights((math.min(a, b), math.max(a, b))) += w
    // Calls and shared mutable state tie methods together outright; domain types and injected fields only attract.
    val strong = Array.tabulate(kept.size)(identity)
    def find(x: Int): Int = if strong(x) == x then x else { strong(x) = find(strong(x)); strong(x) }
    def bind(a: Int, b: Int): Unit = strong(find(a)) = find(b)
    val topics = collection.mutable.Map[String, collection.mutable.ListBuffer[Int]]()
    for m <- kept do
      val i = index(m.name)
      m.types.filterNot(x => hub(typeUse(x))).foreach(x => topics.getOrElseUpdate("t:" + x, collection.mutable.ListBuffer()) += i)
      m.fields.filter(tiesFields).foreach(x => topics.getOrElseUpdate("f:" + x, collection.mutable.ListBuffer()) += i)
      m.calls.filter(x => index.contains(x)).foreach(x => bind(i, index(x)))
    for (topic, members) <- topics if members.size > 1 do
      if topic.endsWith("!") then members.foreach(bind(members.head, _))
      else
        val w = 1.0 / (members.size - 1)
        for a <- members.indices; b <- a + 1 until members.size do link(members(a), members(b), w)
    // Contract the tied methods into one node each, then find the groups of nodes by modularity.
    val roots = kept.indices.map(find).distinct.sorted.zipWithIndex.toMap
    val contracted = collection.mutable.Map[(Int, Int), Double]().withDefaultValue(0.0)
    for ((a, b), w) <- weights do
      val (ra, rb) = (roots(find(a)), roots(find(b)))
      if ra != rb then contracted((math.min(ra, rb), math.max(ra, rb))) += w
    val community = Modularity.communities(roots.size, contracted.toMap)
    val groups = kept.zipWithIndex.groupBy { case (_, i) => community(roots(find(i))) }.values.toList.map(_.map(_._1))
    groups.map { group =>
      val tys = group.flatMap(_.types).filterNot(x => hub(typeUse(x))).groupBy(identity).toList.sortBy(t => (-t._2.size, t._1)).map(_._1).distinct
      Responsibility(group.map(_.name).distinct.sorted, tys)
    }.sortBy(r => (-r.methods.size, r.methods.head))

  /** A group counts as a responsibility when it is more than one method or works on a domain type; typeless one-off
    * methods are miscellany, counted together as one.
    */
  private[plugin] def countResponsibilities(rs: List[Responsibility], grabBag: Boolean = false): Int =
    val real = rs.count(r => r.methods.size > 1 || r.types.nonEmpty)
    val misc = rs.count(r => r.methods.size == 1 && r.types.isEmpty)
    // In a grab-bag the typeless one-off methods are the problem itself, not a harmless remainder: each is a topic.
    real + (if grabBag then misc else math.min(misc, 1))

  private val grabBagName = "(?i).*(util|utils|helper|helpers|common|commons|misc|tools|support)".r

  /** Other packages that use a top-level object: how widely the project reaches into it. Classes and traits are left
    * out: a domain entity or a type used everywhere is expected to be, and says nothing about piled-up helpers; an
    * object (or a file's top-level definitions) used everywhere is a shared bucket.
    */
  private[plugin] def fanIn(all: List[FileFacts]): Map[String, Int] =
    val decls = all.flatMap(_.typeDecls)
    val withClass = decls.filter(_.kind != "object").map(_.sym.stripSuffix("$")).toSet
    val objects = decls.filter(_.kind == "object").map(_.sym.stripSuffix("$")).toSet -- withClass
    all.flatMap(_.classDeps).filter(d => d.fromPkg != d.toPkg && (objects(d.toTop) || d.toTop.contains("$package")))
      .groupBy(_.toTop).view.mapValues(_.map(_.fromPkg).distinct.size).toMap

  /** Fan-in from this many packages makes a class a shared dependency of the whole project. */
  private[plugin] val grabBagFanIn = 5

  /** A utility object (by name) or one the whole project reaches into: whatever it holds is "used by everyone", which
    * is how unrelated code piles up. It is judged by the same rule as any class, with no allowance for plumbing.
    */
  private[plugin] def grabBag(c: ClassFact, fanIn: Map[String, Int]): Boolean =
    grabBagName.matches(c.sym.split('.').last.stripSuffix("$")) || fanIn.getOrElse(c.sym, 0) >= grabBagFanIn

  private[plugin] def responsibilityCount(c: ClassFact, grab: Boolean): Option[Int] =
    if c.methods.size < 4 then None else Some(countResponsibilities(responsibilities(c), grab))

  private def cohesion(c: ClassFact, t: Thresholds, grab: Boolean): List[Finding] =
    if c.methods.size < 4 then Nil
    else
      val rs = responsibilities(c)
      val n = countResponsibilities(rs, grab)
      if n <= t.maxResponsibilities then Nil
      else
        val shown = rs.take(8).zipWithIndex.map { case (r, i) =>
          val topic = if r.types.isEmpty then "" else s"[${r.types.map(_.split('.').last).take(3).mkString(", ")}] "
          s"    ${i + 1}. $topic${r.methods.take(8).mkString(", ")}${if r.methods.size > 8 then ", ..." else ""}"
        }.mkString("\n") + (if rs.size > 8 then s"\n    ... and ${rs.size - 8} more groups" else "")
        val text =
          s"Class `${c.sym}` has $n responsibilities (limit ${t.maxResponsibilities}): its methods work on different " +
            "things and do not share state or call each other.\n" +
            "  Principle: Single Responsibility. A class should have one reason to change; here a change to one of these " +
            "topics has nothing to do with the others, yet they all live in one place.\n" +
            "  The groups, with the domain types each works on (types, fields and helpers used by most methods are ignored):\n" +
            s"$shown\n" +
            "  Fix with one of:\n" +
            "    - Split along these groups: one class or object per group (for a data-access class, one per aggregate), each " +
            "owning what only its methods use.\n" +
            "    - Move a group to the class whose data it works on.\n" +
            (if c.sym.contains("$package") || c.sym.toLowerCase.endsWith("extensions") then
               "    - Extension methods and top-level functions: put each extension next to the type it extends (its receiver), " +
                 "or one `extensions` object per receiver family; a file of extensions on unrelated receivers is a grab-bag " +
                 "whatever its name.\n"
             else "") +
            (if grab then
               "  This is a shared utility (named so, or used from many packages): methods with no domain type are not one " +
                 "harmless remainder here, each is its own topic.\n"
             else "") +
            "  Moving methods behind a facade or `export` does not count: callers still depend on every group.\n" +
            s"  Done when `${c.sym}` has at most ${t.maxResponsibilities} responsibilities."
        List(Finding(s"cohesion ${c.sym}", n, c.file, c.line, text))

  /** A class that is mostly `export` clauses owns no behaviour: it is a facade, and callers still depend on all of
    * what it re-exports.
    */
  private def facade(c: ClassFact, t: Thresholds): List[Finding] =
    if c.exports < t.maxExports || c.exports <= c.methods.size then Nil
    else
      val text =
        s"Class `${c.sym}` is mostly a facade: ${c.exports} of its ${c.exports + c.methods.size} methods are `export` forwarders " +
          s"(limit ${t.maxExports}).\n" +
          "  Principle: avoid Middle Man. An export hides where the code lives without reducing what callers depend on; " +
          "every caller is still coupled to every re-exported member.\n" +
          "  Fix with one of:\n" +
          "    - Let callers use the exported object(s) directly.\n" +
          "    - Give the class behaviour of its own, or split what it exports along the callers' needs.\n" +
          s"  Done when `${c.sym}` has fewer than ${t.maxExports} export forwarders or exports less than it declares."
      List(Finding(s"facade ${c.sym}", c.exports, c.file, c.line, text))
