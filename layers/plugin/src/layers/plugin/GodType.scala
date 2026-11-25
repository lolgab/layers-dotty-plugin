package layers.plugin

/** A type that every part of the program uses, in different ways, grows the fields of all of them: a god object. The
  * same goes for a `<Noun>Service` that offers every operation on one noun. Both are reported only when the users
  * split naturally (they work on different parts), which is what makes them worth splitting.
  */
object GodType:

  private val createVerbs = Set("create", "add", "new", "insert", "register", "crea", "aggiungi", "nuovo", "inserisci", "registra", "save", "salva")
  private val updateVerbs = Set("update", "set", "edit", "modify", "change", "rename", "aggiorna", "modifica", "imposta", "cambia", "toggle", "assign", "assegna")
  private val deleteVerbs = Set("delete", "remove", "archive", "cancel", "drop", "elimina", "rimuovi", "archivia", "annulla", "cancella")
  private val queryVerbs = Set("get", "find", "list", "query", "search", "load", "fetch", "all", "count", "lookup", "ottieni", "cerca", "elenca", "lista", "leggi", "trova", "carica", "by")
  private val suffixes = List("Service", "Manager", "Handler", "Controller", "Facade", "Servizio", "Gestore")

  private def simple(sym: String) = sym.split('.').last

  def findings(facts: List[FileFacts], domain: Set[String], cfg: ArchConfig): List[Finding] =
    dataTypes(facts, domain, cfg) ++ entityServices(facts, domain, cfg)

  /** Overlap coefficient: 1 when one package uses a subset of what the other uses (a detail page reading everything
    * next to a list reading three fields is not a split), 0 when they share nothing.
    */
  private def overlapCoefficient(a: Set[String], b: Set[String]): Double =
    val m = math.min(a.size, b.size)
    if m == 0 then 1.0 else (a & b).size.toDouble / m

  /** Case classes and classes with many fields that several packages use, each for different fields. */
  private def dataTypes(facts: List[FileFacts], domain: Set[String], cfg: ArchConfig): List[Finding] =
    val classFacts = facts.flatMap(_.classes).groupBy(_.sym).view.mapValues(_.head).toMap
    // a call of a method of the class stands for the fields that method (and what it calls) works on
    def expand(cls: String, name: String): Set[String] =
      classFacts.get(cls).map { c =>
        val byName = c.methods.groupBy(_.name).view.mapValues(_.head).toMap
        def go(n: String, seen: Set[String]): Set[String] =
          byName.get(n).map(m => m.fields.map(_.stripSuffix("!")).toSet ++ m.calls.filterNot(seen).flatMap(k => go(k, seen + n + k))).getOrElse(Set.empty)
        go(name, Set(name))
      }.getOrElse(Set.empty)
    val uses = ArchRows.of(facts, "fielduse").groupBy(_.field(2)).map { case (cls, rows) =>
      cls -> rows.groupBy(_.field(1)).view.mapValues(_.flatMap(r => if r.field(4) == "m" then expand(cls, r.field(3)) else Set(r.field(3))).toSet.toList.sorted).toMap
    }
    val users = facts.flatMap(_.classDeps).groupBy(_.toTop).view.mapValues(_.map(_.fromPkg).toSet).toMap
    ArchRows.of(facts, "classinfo").flatMap { r =>
      val (sym, pkg, kind, count, fields) = (r.field(0), r.field(1), r.field(2), r.field(3).toInt, r.list(4))
      if !domain(pkg) || count < math.max(cfg.godTypeMinFields, ArchExtract.fieldFloor) then None
      else
        val referencing = (users.getOrElse(sym, Set.empty) - pkg).toList.sorted
        val perPackage = uses.getOrElse(sym, Map.empty).filter { case (p, _) => p != pkg }.toList.sortBy(_._1)
        val sets = perPackage.map(x => x._1 -> x._2.toSet)
        val similarities = for (a, i) <- sets.zipWithIndex; (b, j) <- sets.zipWithIndex if i < j yield overlapCoefficient(a._2, b._2)
        val overlap = if similarities.isEmpty then 1.0 else similarities.sum / similarities.size
        if referencing.size < cfg.godTypeMinPackages || sets.size < 2 || overlap * 100 > cfg.godTypeMaxOverlap then None
        else
          val lines = perPackage.map { case (p, fs) =>
            val shown = fs.take(8).mkString(", ") + (if fs.size > 8 then s", ... (${fs.size} fields)" else "")
            s"    $p: $shown"
          }.mkString("\n")
          val untouched = fields.filterNot(f => sets.exists(_._2.contains(f)))
          val text =
            s"God type `$sym`: $count fields, referenced from ${referencing.size} other packages (${referencing.take(6).mkString(", ")}" +
              (if referencing.size > 6 then ", ..." else "") + "), and they use different parts of it.\n" +
              "  Fields read or updated per package (outside its own package):\n" +
              s"$lines\n" +
              (if untouched.nonEmpty then s"  Not used outside `$pkg`: ${untouched.take(10).mkString(", ")}${if untouched.size > 10 then ", ..." else ""}\n" else "") +
              s"  Overlap between the packages: ${(overlap * 100).round}% (fields shared, relative to the smaller set; limit ${cfg.godTypeMaxOverlap}%).\n" +
              "  Principle: cohesion by business capability. A type that every use case reads and writes collects the fields of " +
              "all of them: a change for one use case touches the type all the others depend on, and nobody can say what the " +
              "type is. Slices should not be coupled through a shared model that is the union of their needs.\n" +
              "  Fix with one of:\n" +
              "    - Split by use case or workflow: one smaller type per slice, each owning only the fields that slice reads and " +
              "changes (the packages above show the natural split), joined by an id where they describe the same thing.\n" +
              "    - Move fields that only one slice uses into that slice's own type.\n" +
              "  Hiding the fields behind accessors or a trait that all of them implement does not change what depends on what.\n" +
              s"  Done when `$sym` has fewer than ${cfg.godTypeMinFields} fields, or its users share most of them."
          Some(Finding(s"god-type $sym", count, r.file, r.line, text))
    }

  private def verbOf(name: String): String =
    val w = name.takeWhile(c => !c.isUpper && c != '_').toLowerCase
    if w.isEmpty then name.toLowerCase else w

  private def crud(name: String): Option[String] =
    val v = verbOf(name)
    if createVerbs(v) then Some("create") else if updateVerbs(v) then Some("update") else if deleteVerbs(v) then Some("delete")
    else if queryVerbs(v) then Some("query") else None

  /** `<Noun>Service`, `<Noun>Manager`, ...: a class named after one entity that offers create, update, delete and
    * query for it, as separate use cases that share nothing.
    */
  private def entityServices(facts: List[FileFacts], domain: Set[String], cfg: ArchConfig): List[Finding] =
    val typeNames = facts.flatMap(_.typeDecls).map(t => simple(t.sym).stripSuffix("$").toLowerCase).filter(_.length >= 4).toSet
    val privates = facts.flatMap(_.methods).filter(_.priv).map(_.sym).toSet
    def plural(noun: String): Set[String] = Set(noun, noun.stripSuffix("s"), noun.stripSuffix("i"), noun.stripSuffix("e"), noun.stripSuffix("es"), noun.stripSuffix("a") + "i")
    facts.flatMap(_.classes).flatMap { c =>
      val name = simple(c.sym)
      suffixes.find(name.endsWith).flatMap { suffix =>
        val noun = name.stripSuffix(suffix)
        val public = c.methods.map(_.name).distinct.filterNot(m => privates(s"${c.sym}.$m") || m.contains("$"))
        val kinds = public.flatMap(crud).toSet
        if noun.length < 3 || !plural(noun.toLowerCase).exists(typeNames) || public.size < cfg.godServiceMinMethods || kinds.size < 3 then None
        else
          val groups = ClassMetrics.responsibilities(c).filter(_.methods.size > 1)
          if groups.size < 2 then None
          else
            val shown = groups.take(6).zipWithIndex.map { case (g, i) => s"    ${i + 1}. ${g.methods.take(8).mkString(", ")}${if g.methods.size > 8 then ", ..." else ""}" }.mkString("\n")
            val text =
              s"Entity service `${c.sym}`: ${public.size} public operations on one noun (`$noun`) covering ${kinds.toList.sorted.mkString(", ")}, in ${groups.size} groups that do not share state or call each other.\n" +
                s"$shown\n" +
                "  Principle: avoid entity services and god objects. A service named after an entity collects every use case that " +
                "touches it (create, change, delete, query for each screen or report), so unrelated features change in the same " +
                "class and everything that needs one operation depends on all of them.\n" +
                "  Fix with one of:\n" +
                "    - Split by use case or workflow: one class or function per slice (`Register$noun`, `Close$noun`, `$noun" + "Report`), each owning " +
                "the data and the rules of its own use case.\n" +
                "    - Move rules that are about the entity itself into the entity; leave in the service only what coordinates.\n" +
                "  Renaming the class or moving operations behind a facade does not count: the groups above are what is judged.\n" +
                s"  Done when `${c.sym}` has fewer than ${cfg.godServiceMinMethods} public operations or they fall in one group."
            Some(Finding(s"god-type ${c.sym}", public.size, c.file, c.line, text))
      }
    }
