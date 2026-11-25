package layers.plugin

/** A handler that parses the request, queries and writes the database, calls an external service and renders HTML
  * mixes validation, state change and side effects: it can neither be tested nor changed in one piece.
  */
object LayerMix:

  private val layerKinds = Set("persistence", "rendering", "web", "integration")

  /** A method has to call at least this many different things of the domain for it to count as a layer it touches. */
  private val domainMinCalls = 2

  private val titles = Map(
    "persistence" -> "persistence", "rendering" -> "rendering/view", "web" -> "web/routing",
    "integration" -> "external service", "domain" -> "domain"
  )

  /** What a method mixes: the layers it touches and what it uses in each, with the method measured together with its
    * single-use helpers.
    */
  private[plugin] final case class Mix(effective: ClassMetrics.Effective, used: List[(String, List[String])])

  private def related(a: String, b: String) = a == b || a.startsWith(b + ".") || b.startsWith(a + ".")

  def findings(facts: List[FileFacts], domain: Set[String], cfg: ArchConfig): List[Finding] =
    measure(facts, domain, cfg).flatMap(finding(_, cfg)).sortBy(f => (f.file, f.line, f.key))

  /** Every method that touches at least one layer, with the layers (in the report, how close to the limit it is). */
  private[plugin] def measure(facts: List[FileFacts], domain: Set[String], cfg: ArchConfig): List[Mix] =
    val refs = ArchRows.of(facts, "ref").groupBy(_.field(0))
    val methods = facts.flatMap(_.methods)
    def kindOf(pkg: String, kind: String): Option[String] =
      if layerKinds(kind) then Some(kind)
      else if kind == "project" && domain(pkg) then Some("domain")
      else if kind == "domain" then Some("domain")
      else None
    def ownKind(pkg: String): Option[String] =
      if domain(pkg) then Some("domain")
      else PackageKinds.kind(pkg, cfg, internal = true).filter(k => layerKinds(k) && k != "web")
    ClassMetrics.effective(methods).flatMap { e =>
      val syms = e.fact.sym :: e.absorbed
      val rows = syms.flatMap(s => refs.getOrElse(s, Nil))
      // layer -> what the method uses in it
      val used = scala.collection.mutable.LinkedHashMap[String, scala.collection.mutable.LinkedHashSet[String]]()
      // an orchestrator inside an integration package (the Telegram or SIAN sync service) is the integration, not a
      // caller of an external service: only calls to other integrations count
      val ownPkg = rows.headOption.map(_.field(1))
      val inIntegration = ownPkg.exists(p => !domain(p) && ownKind(p).contains("integration"))
      for r <- rows do
        kindOf(r.field(2), r.field(3)).filterNot(k => k == "integration" && inIntegration && ownPkg.exists(related(_, r.field(2)))).foreach { k =>
          val names = r.list(4).map(n => s"${r.field(2).split('.').last}.$n")
          used.getOrElseUpdate(k, scala.collection.mutable.LinkedHashSet()) ++= names
        }
      // the package the method is in is a layer it belongs to: a view that queries the database mixes the two
      if rows.nonEmpty then ownKind(rows.head.field(1)).filterNot(_ == "integration").foreach(k => used.getOrElseUpdate(k, scala.collection.mutable.LinkedHashSet()))
      // one call into the domain is a helper (raising an error, formatting a number), not domain logic done in the handler
      if used.get("domain").exists(n => n.nonEmpty && n.size < domainMinCalls) then used.remove("domain")
      if used.isEmpty then None else Some(Mix(e, used.toList.map { case (k, v) => k -> v.toList }))
    }

  private def finding(mix: Mix, cfg: ArchConfig): Option[Finding] =
    val e = mix.effective
    val used = mix.used.toMap
    if used.size < cfg.maxLayersPerMethod || e.fact.cyclomatic < cfg.layerMixMinCyclomatic then None
    else
      {
        val m = e.fact
        val order = List("web", "persistence", "integration", "rendering", "domain")
        val lines = order.filter(used.contains).map { k =>
          val names = used(k)
          val shown = if names.isEmpty then "(the method lives in this layer)" else names.take(6).mkString(", ") + (if names.size > 6 then s", ... (${names.size} in all)" else "")
          s"    ${titles(k)}: $shown"
        }.mkString("\n")
        val note = if e.absorbed.isEmpty then "" else s" (counting ${e.absorbed.size} single-use helper${if e.absorbed.size == 1 then "" else "s"}; splitting a method into pieces only it uses does not separate the layers)"
        val text =
          s"Method `${DomainPurity.clean(m.sym)}` mixes ${used.size} layers (limit ${cfg.maxLayersPerMethod}) and has ${m.cyclomatic - 1} branches of its own$note:\n" +
            s"$lines\n" +
            "  Principle: a handler should orchestrate, not do everything. Validating input, changing state, talking to other " +
            "systems and presenting the result change for different reasons and are tested differently; in one method every " +
            "change risks the others and none can be tested alone.\n" +
            "  Fix with one of:\n" +
            "    - Separate the steps: validate and decide in the domain (pure, returns a result), apply the state change in " +
            "one place, and perform side effects (service calls, notifications) after it, from the application layer.\n" +
            "    - Move rendering out: the handler returns data, a view function turns it into HTML.\n" +
            "    - Give each use case its own handler that talks to the layers it needs.\n" +
            s"  Done when `${DomainPurity.clean(m.sym)}` touches fewer than ${cfg.maxLayersPerMethod} of these layers or has fewer than ${cfg.layerMixMinCyclomatic - 1} branches of its own (a straight-line orchestrator is not judged)."
        Some(Finding(s"layer-mix ${m.sym}", used.size, m.file, m.line, text, e.ranges))
      }
