package layers.plugin

/** The architecture checks that judge where code sits and what it knows about: domain purity and identity, god
  * types, scattered transitions, bloated handlers and rules by relationship. Run from the design phase on the facts of
  * the whole project.
  */
object ArchChecks:

  def findings(facts: List[FileFacts], graph: PackageGraph, cfg: ArchConfig): List[Finding] =
    val domain = Domains.infer(cfg, facts, graph)
    (DomainPurity.findings(facts, domain, cfg) ++
      DomainIdentity.findings(facts, domain, cfg) ++
      GodType.findings(facts, domain, cfg) ++
      ScatteredTransition.findings(facts, domain, cfg) ++
      LayerMix.findings(facts, domain, cfg) ++
      RuleByRelationship.findings(facts, domain, cfg)).sortBy(f => (f.file, f.line, f.key))

  /** Sources whose architecture facts were extracted under other options (`identityTypes`, `layerKinds`): they say
    * nothing about the current ones until recompiled.
    */
  def stale(facts: List[FileFacts], cfg: ArchConfig): List[String] =
    facts.filter(f => f.arch.exists(a => a.kind == "cfg" && a.field(0) != cfg.extractionKey)).map(_.source).sorted
