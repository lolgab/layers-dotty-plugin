package layers.plugin

/** A measured design problem: `value` is what was measured, and the finding exists because it is over the limit.
  *
  * @param key
  *   stable identity, e.g. `cyclomatic app.Service.run` or `sdp app -> util`
  * @param value
  *   measured amount; 0 for yes/no findings
  */
final case class Finding(key: String, value: Int, file: String, line: Int, message: String)

/** Accepted legacy findings, each with the worst value that is still accepted. Anything new or worse fails the build.
  * Tooling only lowers or removes entries, so raising one is always a visible, deliberate edit of the file.
  */
object Baseline:

  private val header =
    "# layers design baseline: accepted legacy findings and their worst accepted value.\n" +
      "# Tooling only lowers or removes entries (-P:layers:updateBaseline); raising one is a manual edit.\n"

  private val separator = " = "

  def parse(content: String): Map[String, Int] =
    content.linesIterator.map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).map { line =>
      val i = line.lastIndexOf(separator)
      val value = if i < 0 then None else line.drop(i + separator.length).trim.toIntOption
      value match
        case Some(v) => line.take(i).trim -> v
        case None    => line -> 0
    }.toMap

  def render(entries: Map[String, Int]): String =
    header + entries.toList.sortBy(_._1).map { case (k, v) => if v == 0 then k + "\n" else s"$k$separator$v\n" }.mkString

  /** @param failing
    *   findings that are new or worse than the baseline allows
    * @param stale
    *   human readable baseline entries that improved or vanished but are not locked in yet
    * @param write
    *   new baseline content when updating
    */
  final case class Outcome(failing: List[Finding], stale: List[String], write: Option[Map[String, Int]])

  /** @param baseline
    *   None when there is no baseline file yet
    * @param update
    *   with no baseline file: record every current finding. With one: lower or drop entries, never add or raise
    * @param unchecked
    *   baseline keys that were not evaluated in this run (e.g. a check that is skipped): kept as they are
    */
  def evaluate(
      findings: List[Finding],
      baseline: Option[Map[String, Int]],
      update: Boolean,
      unchecked: String => Boolean = _ => false
  ): Outcome =
    val current = findings.groupBy(_.key).view.mapValues(_.map(_.value).max).toMap
    val base = baseline.getOrElse(Map.empty)
    val (skipped, checked) = base.partition { case (k, _) => unchecked(k) }
    val kept: Map[String, Int] =
      if baseline.isEmpty then current
      else checked.collect { case (k, v) if current.contains(k) => k -> math.min(v, current(k)) } ++ skipped
    val allowed = if baseline.isEmpty && !update then Map.empty[String, Int] else if update then kept else base
    val failing = findings.filter(f => allowed.get(f.key).forall(f.value > _))
    if update then Outcome(failing, Nil, Some(kept))
    else
      val stale = checked.toList.sortBy(_._1).flatMap { case (k, v) =>
        current.get(k) match
          case None             => Some(s"$k: fixed (was $v)")
          case Some(c) if c < v => Some(s"$k: improved from $v to $c")
          case _                => None
      }
      Outcome(failing, stale, None)
