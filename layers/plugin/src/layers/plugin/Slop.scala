package layers.plugin

/** Detectors for code that adds volume but no design: copies, forwarding wrappers, interfaces nobody needs. */
object Slop:

  def findings(facts: Iterable[FileFacts], t: Thresholds): List[Finding] =
    val all = facts.toList
    (passThroughs(all.flatMap(_.passThroughs)) ++
      clones(all.flatMap(_.clones), math.max(t.minCloneSize, SlopMetrics.cloneFloor)) ++
      singleImplementations(all.flatMap(_.typeDecls))).sortBy(f => (f.file, f.line, f.key))

  private def passThroughs(facts: List[PassThroughFact]): List[Finding] =
    facts.map { p =>
      val text =
        s"Method `${p.sym}` only forwards its parameters to `${p.target}`.\n" +
          "  Principle: avoid Middle Man. A wrapper that adds no behaviour adds a name to learn and a place to change.\n" +
          "  Fix with one of:\n" +
          s"    - Delete `${p.sym}` and call `${p.target}` directly.\n" +
          "    - If the wrapper is meant to hide the dependency, make it earn that: give it behaviour of its own " +
          "(validation, translation, a narrower signature) or declare the abstraction as a trait.\n" +
          s"  Done when `${p.sym}` no longer exists or does more than forward."
      Finding(s"passthrough ${p.sym}", 1, p.file, p.line, text)
    }

  private def clones(facts: List[CloneFact], minSize: Int): List[Finding] =
    facts.filter(_.size >= minSize).groupBy(_.hash).values.toList.filter(_.map(_.sym).distinct.size > 1).map { group =>
      val members = group.distinctBy(_.sym).sortBy(m => (m.file, m.line))
      val size = members.map(_.size).max
      val locations = members.map(m => s"    ${m.file}:${m.line}  ${m.sym}").mkString("\n")
      val text =
        s"Duplicated code: ${members.size} methods have the same logic ($size syntax nodes), identical apart from " +
          "local names and literal values:\n" +
          s"$locations\n" +
          "  Principle: Don't Repeat Yourself. Copies drift apart: a fix made in one is missed in the others.\n" +
          "  Fix: extract one method parameterised by what differs (values, or a function), put it where all " +
          "callers can reach it (common trait, object or package), and delete the copies.\n" +
          "  Done when only one of these methods remains."
      Finding(s"clone ${members.map(_.sym).sorted.mkString(" ~ ")}", size, members.head.file, members.head.line, text)
    }

  /** An abstraction needs a reason: a package boundary it inverts, or several implementations. A trait whose only
    * implementation sits in the same package has neither.
    */
  private def singleImplementations(types: List[TypeDecl]): List[Finding] =
    val concrete = Set("class", "object", "anon")
    types
      .filter(a => (a.kind == "trait" || a.kind == "abstract") && !a.sealedType && !a.generic && !a.local)
      .flatMap { a =>
        types.filter(_.parents.contains(a.sym)) match
          case List(impl) if concrete(impl.kind) && impl.pkg == a.pkg =>
            val kind = if a.kind == "trait" then "Trait" else "Abstract class"
            val text =
              s"$kind `${a.sym}` has a single implementation, `${impl.sym}`, in the same package " +
                s"(${impl.file}:${impl.line}).\n" +
                "  Principle: avoid speculative generality. An interface earns its place by inverting a package " +
                "dependency or by having several implementations; this one does neither.\n" +
                "  Fix with one of:\n" +
                s"    - Delete `${a.sym}` and use `${impl.sym}` directly.\n" +
                s"    - If it is meant as a boundary, move `${impl.sym}` into the package that should depend on " +
                s"`${a.sym}` instead of the one that owns it.\n" +
                "    - If implementations exist in other modules, baseline this finding.\n" +
                s"  Done when `${a.sym}` is removed, or has an implementation in another package or a second one."
            Some(Finding(s"single-impl ${a.sym}", 1, a.file, a.line, text))
          case _ => None
      }
