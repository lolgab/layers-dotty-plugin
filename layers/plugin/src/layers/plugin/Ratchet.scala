package layers.plugin

/** Enforces design thresholds on the code a change touches, and only reports the rest. Legacy code is not rewritten to
  * satisfy per-method limits: it is cleaned when it is worked on, by whoever is already reading it.
  */
object Ratchet:

  /** Findings about a whole class, file or package: they are judged where the declaration is. */
  private val structural = Set("cohesion", "facade", "single-impl", "filesize")

  /** Findings that are about the project as a whole, not about a place in a file. */
  private val global = Set("sdp")

  /** @return
    *   (findings that fail the build, findings in code that did not change)
    */
  def split(findings: List[Finding], changes: GitSignals.Changes): (List[Finding], List[Finding]) =
    findings.partition { f =>
      if global(f.kind) then true
      else if structural(f.kind) then changes.touches(f.file, List((f.line, f.line)))
      else changes.touches(f.file, f.lines)
    }

  /** One line per kind, for a warning that says what was left alone. */
  def summary(existing: List[Finding], rev: String): String =
    val byKind = existing.groupBy(_.kind).toList.sortBy(-_._2.size).map { case (k, fs) => s"${fs.size} $k" }
    s"[layers] ${existing.size} existing findings are in code not changed since $rev and are not enforced " +
      s"(${byKind.mkString(", ")}). They fail the build as soon as the code around them is touched."
