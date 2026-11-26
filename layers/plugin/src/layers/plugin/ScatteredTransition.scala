package layers.plugin

/** The same state change written out in several places: `order.copy(status = Closed)` in three handlers. The rule
  * ("closing an order sets the status") lives in none of them, so each can forget a precondition or a second field.
  * That is an anemic model: the transition belongs to the type.
  */
object ScatteredTransition:

  private final case class Site(file: String, line: Int, member: String, top: String)

  def findings(facts: List[FileFacts], domain: Set[String], cfg: ArchConfig): List[Finding] =
    ArchRows.of(facts, "copy").filter(r => DomainPurity.inDomain(r.field(2), domain)).filterNot { r =>
      // inside the type or its companion the transition is where it should be
      val cls = r.field(2)
      r.field(0).startsWith(cls + ".") || r.field(0) == cls
    }.groupBy(r => (r.field(2), r.field(3), r.field(4))).toList.sortBy(_._1).flatMap { case ((cls, field, value), rows) =>
      val sites = rows.map(r => Site(r.file, r.line, r.field(0), r.field(1))).distinctBy(_.member).sortBy(s => (s.file, s.line))
      if sites.size < cfg.minTransitionSites || sites.map(_.top).distinct.size < 2 then None
      else
        val shown = sites.take(10).map(s => s"    ${s.file}:${s.line}  ${DomainPurity.clean(s.member)}").mkString("\n") + (if sites.size > 10 then s"\n    ... and ${sites.size - 10} more" else "")
        val name = cls.split('.').last
        val text =
          s"State transition `$field = $value` of `$name` is written out in ${sites.size} methods of ${sites.map(_.top).distinct.size} classes.\n" +
            s"$shown\n" +
            "  Principle: anemic domain model. When every caller does `x.copy(field = value)` the rule for the change lives in " +
            "no place: each caller decides alone whether it is allowed, what else must change with it, and what to record. " +
            "Things that change together belong together: the transition is behaviour of the type.\n" +
            "  Fix: add a method to `" + name + "` (or its companion) that performs the transition, with its preconditions, and " +
            s"call it from the sites above (e.g. `def ${field}To${value.split('.').last.capitalize.filter(_.isLetterOrDigit)}: $name = copy($field = $value)`, or one that returns `Either`/`Option` when it is " +
            "only valid from some states).\n" +
            "  Putting the `copy` in a helper that is not on the type, or calling it through a differently named variable, still " +
            "leaves the rule outside the type.\n" +
            s"  Done when fewer than ${cfg.minTransitionSites} methods outside `$name` write this transition."
        Some(Finding(s"scattered-transition $cls.$field=${value.replace(' ', '_').take(40)}", sites.size, sites.head.file, sites.head.line, text, sites.map(s => (s.line, s.line))))
    }
