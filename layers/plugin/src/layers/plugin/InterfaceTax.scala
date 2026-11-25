package layers.plugin

/** Indirection without a reason. Complements the `single-impl` check (a trait whose only implementation is in the same
  * package) with the two cases it cannot see:
  *
  *   - `interface-tax <type>`: a trait or abstract class with exactly one implementation, in another package, and
  *     exactly one class that uses it, where the package dependency it inverts is not needed: the user could depend on the
  *     implementation without creating a cycle. The interface only adds a name and a hop. (An interface that breaks a
  *     real cycle, or that several classes are decoupled from, is not flagged.)
  *   - `forwarder-class <class>`: a class whose every method only forwards to the same other class (a repository or
  *     service kept for indirection), and which has exactly one caller class. A wrapper with several callers is a
  *     Middle Man that at least has a name callers agree on; with one caller it is a detour.
  */
object InterfaceTax:

  def findings(facts: List[FileFacts], graph: PackageGraph): List[Finding] =
    val deps = facts.flatMap(_.classDeps)
    (interfaces(facts.flatMap(_.typeDecls), deps, graph) ++
      forwarders(facts.flatMap(_.classes), facts.flatMap(_.passThroughs), deps)).sortBy(f => (f.file, f.line, f.key))

  private val concrete = Set("class", "object", "anon")

  private def interfaces(types: List[TypeDecl], deps: List[ClassDep], graph: PackageGraph): List[Finding] =
    val usersOf = deps.groupBy(_.toTop)
    types
      .filter(a => (a.kind == "trait" || a.kind == "abstract") && !a.sealedType && !a.generic && !a.local && Slices.isTopLevel(a))
      .flatMap { a =>
        types.filter(_.parents.contains(a.sym)) match
          // the same-package case is the `single-impl` finding
          case List(impl) if concrete(impl.kind) && impl.pkg != a.pkg =>
            val implTop = Slices.topOf(impl)
            val users = usersOf.getOrElse(a.sym, Nil).filter(d => d.fromTop != implTop && d.fromTop != a.sym).groupBy(_.fromTop).toList.sortBy(_._1)
            val userPkgs = users.map(_._2.head.fromPkg).distinct
            // The interface is justified when the user cannot depend on the implementation without a cycle.
            val needed = userPkgs.exists(p => p == impl.pkg || SharedOwnership.reaches(graph, impl.pkg, p))
            if users.size == 1 && !needed then
              val kind = if a.kind == "trait" then "Trait" else "Abstract class"
              val where = users.headOption.map { case (top, ds) => s"used by one class only, `$top` (${ds.head.file}:${ds.head.line})" }.getOrElse("")
              val text =
                s"$kind `${a.sym}` has a single implementation, `${impl.sym}` (${impl.file}:${impl.line}), and is $where.\n" +
                  "  Principle: no indirection without a reason. The interface does not invert a dependency that would " +
                  s"otherwise create a cycle (`${impl.pkg}` does not depend on ${userPkgs.map(p => s"`$p`").mkString(", ")}), " +
                  "and no second class is decoupled by it: it only adds a name to learn and a hop to follow.\n" +
                  "  Fix with one of:\n" +
                  s"    - Delete `${a.sym}` and let the user take `${impl.sym}` directly. Test it with the real class (an in-memory database, a fake endpoint), not a mock of an interface.\n" +
                  "    - Keep it if a second implementation exists in production code, and baseline this finding until then.\n" +
                  s"  Adding a second implementation only to satisfy this check (a stub, a `Noop`) does not count: it has to be used.\n" +
                  s"  Done when `${a.sym}` is removed, or has a second implementation, or a second class uses it."
              Some(Finding(s"interface-tax ${a.sym}", 1, a.file, a.line, text, List((a.line, a.line))))
            else None
          case _ => None
      }

  private def owner(sym: String): String = sym.take(sym.lastIndexOf('.'))

  private def forwarders(classes: List[ClassFact], passThroughs: List[PassThroughFact], deps: List[ClassDep]): List[Finding] =
    val bySym = passThroughs.groupBy(p => owner(p.sym))
    val callersOf = deps.groupBy(_.toTop)
    classes.flatMap { c =>
      val names = c.methods.map(_.name).distinct
      val forwarded = bySym.getOrElse(c.sym, Nil)
      val targets = forwarded.map(p => owner(p.target)).distinct
      val top = c.sym
      val callers = callersOf.getOrElse(top, Nil).map(_.fromTop).filter(_ != top).distinct.sorted
      if names.size >= 2 && names.forall(n => forwarded.exists(_.sym == c.sym + "." + n)) && targets.size == 1 && callers.size == 1 then
        val target = targets.head
        val who = s"its only caller is `${callers.head}`"
        val text =
          s"Class `${c.sym}` only forwards: all ${names.size} of its methods hand their parameters to `$target`, and $who.\n" +
            "  Principle: no indirection without a reason (a repository or service kept as a layer that adds nothing). " +
            "A class whose methods all delegate has no behaviour of its own to name; its caller could ask `" + target + "` directly.\n" +
            "  Fix with one of:\n" +
            s"    - Delete `${c.sym}` and call `$target` from the caller.\n" +
            "    - Give it a reason: behaviour of its own (validation, translation to the domain, caching), so it is more than the sum of its calls.\n" +
            s"  Done when `${c.sym}` is removed or one of its methods does more than forward."
        Some(Finding(s"forwarder-class ${c.sym}", names.size, c.file, c.line, text, List((c.line, c.line))))
      else None
    }
