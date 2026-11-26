package layers.plugin

/** An aggregate that holds a whole collection of entities only to count it or ask whether it contains one: the rule
  * ("at most 5 lots per tank", "no two with the same code") is about a number or a uniqueness, so the relationship is
  * modelled with more than the rule needs.
  */
object RuleByRelationship:

  private val counting = Set("size", "length", "isEmpty", "nonEmpty", "contains", "exists", "knownSize", "sizeIs", "lengthIs", "count", "forall")

  def findings(facts: List[FileFacts], domain: Set[String], cfg: ArchConfig): List[Finding] =
    if !cfg.ruleByRelationship then Nil
    else
      val uses = ArchRows.of(facts, "colluse").groupBy(_.field(0))
      ArchRows.of(facts, "collfield").flatMap { f =>
        val (cls, field, elem, derives) = (f.field(0), f.field(1), f.field(2), f.field(3) == "1")
        val mine = uses.getOrElse(cls, Nil).filter(u => u.field(1) == field || u.field(1) == "*")
        val inside = mine.filter(u => u.field(3).startsWith(cls + "."))
        if !DomainPurity.inDomain(cls, domain) || derives || mine.isEmpty || inside.isEmpty || !mine.forall(u => counting(u.field(2))) then None
        else
          val ops = mine.map(_.field(2)).distinct.sorted
          val sites = mine.map(u => s"    ${u.file}:${u.line}  ${u.field(3)}: .${u.field(2)}").distinct.take(8).mkString("\n")
          val text =
            s"`$cls` holds `$field`, a collection of `${elem.split('.').last}`, only to ${ops.mkString("/")} it.\n" +
              s"$sites\n" +
              "  Principle: model rules, not relationships. An aggregate that loads a whole collection to check a count or a " +
              "uniqueness carries (and must keep consistent) far more than the rule needs.\n" +
              "  Fix with one of:\n" +
              "    - Keep only what the rule needs: the count, or the set of keys.\n" +
              "    - Enforce the rule in a query or a database constraint where the data is, and drop the collection.\n" +
              s"  Done when `$cls` no longer holds `$field` only for these checks."
          Some(Finding(s"rule-by-relationship $cls.$field", ops.size, f.file, f.line, text))
      }
