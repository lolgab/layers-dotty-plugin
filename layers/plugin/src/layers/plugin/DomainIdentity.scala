package layers.plugin

/** Authorization (who may do it) belongs to the application layer; the domain only knows what is valid. A domain
  * method that takes the acting user, a session or the request needs a request to run and mixes the two.
  */
object DomainIdentity:

  private final case class Hit(file: String, line: Int, symbol: String, what: String, tpe: String, reason: String)

  private def pkgOf(full: String): String = full.take(math.max(full.lastIndexOf('.'), 0))

  def findings(facts: List[FileFacts], domain: Set[String], cfg: ArchConfig): List[Finding] =
    if domain.isEmpty then Nil
    else
      def judge(tpe: String, name: String, compared: Boolean): Option[String] =
        Identity.strength(tpe, pkgOf(tpe), cfg, domain) match
          case "strong" => Some(s"`${tpe.split('.').last}` is a request or identity type")
          case _ =>
            if Identity.actorName(name) then Some(s"parameter `$name` of type `${tpe.split('.').last}` is the acting user")
            else if compared then Some(s"`${tpe.split('.').last}` is compared with ownership data to decide who may do it")
            else None
      val params = ArchRows.of(facts, "identparam").filter(r => DomainPurity.inDomain(r.field(0), domain)).flatMap { r =>
        judge(r.field(2), r.field(1), r.field(4) == "1").map(why => Hit(r.file, r.line, r.field(0), s"parameter `${r.field(1)}: ${r.field(2).split('.').last}`" + (if r.field(3) == "1" then " (using)" else ""), r.field(2), why))
      }
      val fields = ArchRows.of(facts, "identfield").filter(r => DomainPurity.inDomain(r.field(0), domain)).flatMap { r =>
        val tpe = r.field(2)
        val why = Identity.strength(tpe, pkgOf(tpe), cfg, domain) match
          case "strong" => Some(s"`${tpe.split('.').last}` is a request or identity type")
          case _        => if Identity.actorName(r.field(1)) then Some(s"field `${r.field(1)}` is the acting user") else None
        why.map(w => Hit(r.file, r.line, r.field(0), s"field `${r.field(1)}: ${tpe.split('.').last}`", tpe, w))
      }
      (params ++ fields).groupBy(_.symbol).toList.sortBy(_._1).map { case (sym, hits) =>
        val sorted = hits.distinctBy(h => (h.what, h.line)).sortBy(h => (h.file, h.line))
        val list = sorted.map(h => s"    ${h.file}:${h.line}  ${h.what}: ${h.reason}").mkString("\n")
        val text =
          s"Domain code `${DomainPurity.clean(sym)}` takes the acting identity or request context: ${sorted.size} place${if sorted.size == 1 then "" else "s"}.\n" +
            s"$list\n" +
            "  Principle: authorization (who may do it) belongs in the application layer, validity (what is valid) in the " +
            "domain. A domain method that takes the user, session or request cannot be run or tested without one, and " +
            "every rule in it can silently grow a role check.\n" +
            "  Fix with one of:\n" +
            "    - Check the permission in the application layer (the handler or use case) before calling the domain, and " +
            "pass only what the rule needs: a value (the owner id to compare with), or the result of the check.\n" +
            "    - If the rule is a real invariant about ownership, model it as data the rule can see (an `ownerId` field " +
            "and the id to check) instead of the acting user.\n" +
            "  A domain `User` that is only data (an owner, an author) is fine: pass it where it is a field of the thing " +
            "being described, not as the person doing the action. Renaming the parameter does not help: comparing it with " +
            "ownership data is what is judged.\n" +
            s"  Done when `${DomainPurity.clean(sym)}` no longer takes it."
        Finding(s"domain-identity $sym", sorted.size, sorted.head.file, sorted.head.line, text, sorted.map(h => (h.line, h.line)).distinct)
      }
