package layers.plugin

/** The domain decides what is valid and computes results; it does not look at the clock, draw random numbers, touch
  * files, the environment or the network, write logs, or use the database or the web framework. Code that does cannot
  * be tested without the world, and gives a different answer every time it runs.
  */
object DomainPurity:

  /** What the standard library lets a function reach without being given it. */
  private val ambient = Set("clock", "random", "filesystem", "env", "logging", "console", "thread")

  private val what = Map(
    "clock" -> "reads the clock",
    "random" -> "draws random values or ids",
    "filesystem" -> "reads or writes files or resources",
    "env" -> "reads the environment or runs processes",
    "logging" -> "writes logs",
    "console" -> "writes to the console",
    "thread" -> "sleeps or starts threads",
    "persistence" -> "uses the database layer",
    "rendering" -> "builds HTML or documents",
    "web" -> "depends on the web framework",
    "integration" -> "calls an external service or the network"
  )

  private final case class Site(file: String, line: Int, member: String, category: String, label: String, via: Option[String]):
    def labels: List[String] = label.split(", ").toList

  private def exempt(cfg: ArchConfig, names: String*): Boolean =
    cfg.pureExempt.exists(e => names.exists(n => n == e || n.startsWith(e + ".") || n.startsWith(e + "$")))

  def findings(facts: List[FileFacts], domain: Set[String], cfg: ArchConfig): List[Finding] =
    if domain.isEmpty then Nil
    else
      val rows = ArchRows.of(facts, "impure")
      val direct = rows.filter(r => domain(r.field(3)) && !exempt(cfg, r.field(0), r.field(4))).map { r =>
        Site(r.file, r.line, r.field(0), r.field(1), r.field(2), None)
      }
      val tops = rows.filter(r => domain(r.field(3))).map(r => r.field(0) -> r.field(4)).toMap
      val all = direct ++ transitive(facts, rows, domain, cfg)
      all.groupBy(s => tops.getOrElse(s.member, owner(s.member))).toList.sortBy(_._1).map { case (top, sites) => finding(top, merge(sites)) }

  /** One site per member, kind of use and helper: a call that touches three library types is one use, not three. */
  private def merge(sites: List[Site]): List[Site] =
    sites.groupBy(s => (s.member, s.category, s.via)).values.toList.map { group =>
      val first = group.minBy(s => (s.file, s.line))
      first.copy(label = group.sortBy(s => (s.line, s.label)).map(_.label).distinct.take(4).mkString(", "))
    }.sortBy(s => (s.file, s.line, s.member))

  private[plugin] def clean(sym: String): String = sym.replace("$.", ".").stripSuffix("$")

  private def owner(member: String): String = member.take(math.max(member.lastIndexOf('.'), 0))

  def inDomain(sym: String, domain: Set[String]): Boolean = domain.exists(p => sym.startsWith(p + "."))

  /** A domain method that calls a helper outside the domain which reads the clock moves the call, it does not remove
    * it: the helper is followed up to three calls deep (through code that is not domain; infrastructure packages are
    * reported where the domain touches them).
    */
  private def transitive(facts: List[FileFacts], rows: List[ArchFact], domain: Set[String], cfg: ArchConfig): List[Site] =
    val methods = facts.flatMap(_.methods).groupBy(_.sym).map { case (k, ms) => k -> ms.head }
    val calls = facts.flatMap(_.methods).groupBy(_.sym).view.mapValues(_.flatMap(_.calls).distinct).toMap
    val direct = rows.filter(r => ambient(r.field(1))).groupBy(_.field(0)).view.mapValues(_.head).toMap
    val memo = scala.collection.mutable.Map[String, Option[(String, String)]]()
    def reach(m: String, depth: Int, seen: Set[String]): Option[(String, String)] =
      memo.get(m).getOrElse {
        val r =
          if exempt(cfg, m) || inDomain(m, domain) then None
          else direct.get(m).map(x => (x.field(1), x.field(2))).orElse {
            if depth == 0 then None
            else calls.getOrElse(m, Nil).filterNot(seen).view.flatMap(c => reach(c, depth - 1, seen + m)).headOption
          }
        if depth == 3 then memo(m) = r
        r
      }
    for
      m <- methods.values.toList if inDomain(m.sym, domain) && !exempt(cfg, m.sym)
      c <- calls.getOrElse(m.sym, Nil).distinct if !inDomain(c, domain)
      (category, label) <- reach(c, 3, Set(m.sym))
    yield Site(m.file, m.line, m.sym, category, label, Some(c))

  private def finding(top: String, sites: List[Site]): Finding =
    val shown = sites.take(10).map { s =>
      val verb = what.getOrElse(s.category, s.category)
      val via = s.via.map(v => s" (through `${clean(v)}`, which is outside the domain)").getOrElse("")
      s"    ${s.file}:${s.line}  ${clean(s.member)}: $verb, ${s.labels.map(l => s"`$l`").mkString(", ")}$via"
    }.mkString("\n") + (if sites.size > 10 then s"\n    ... and ${sites.size - 10} more" else "")
    val text =
      s"Domain code `$top` is not pure: ${sites.size} use${if sites.size == 1 then "" else "s"} of the outside world.\n" +
        s"$shown\n" +
        "  Principle: the domain decides what is valid and computes results from its inputs. A rule that reads the clock, " +
        "draws an id, writes a file or calls a service gives a different answer on every run and cannot be tested " +
        "without the world. Time, randomness, policies and I/O are the application layer's business.\n" +
        "  Fix with one of:\n" +
        "    - Pass the value in as a parameter (`today: LocalDate`, `nextId: Id`, a policy object) and let the caller, " +
        "in the application layer, read the clock or draw the id once. A default argument such as `= LocalDate.now()` " +
        "still reads the clock from the domain: make the parameter required.\n" +
        "    - Return what should happen (a request to send, a record to write) and let the application layer perform it.\n" +
        "    - Move code that is really about the outside world (HTTP calls, files, SQL, HTML, logging) out of the domain " +
        "package into the application or an adapter package.\n" +
        "  Taking a `Clock`, `Random`, connection or logger as a constructor or method parameter is the same dependency: " +
        "pass the value, not the source of it. Wrapping the call in a helper outside the domain does not help either: " +
        "the helper is followed.\n" +
        "  Where a symbol is a deliberate seam, exempt it by qualified name with `-P:layers:pureExempt=<name>[,<name>]` " +
        "(a class, object or method; the exemption is visible in the build).\n" +
        s"  Done when `$top` uses none of the above."
    val first = sites.minBy(s => (s.file, s.line))
    Finding(s"domain-purity $top", sites.size, first.file, first.line, text, sites.map(s => (s.line, s.line)).distinct)
