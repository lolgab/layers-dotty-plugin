package layers.plugin

/** Limits for method and class measurements. A measurement over its limit is a finding. */
final case class Thresholds(
    maxCyclomatic: Int = 10,
    maxCognitive: Int = 15,
    maxMethodLines: Int = 60,
    maxParams: Int = 5,
    maxNesting: Int = 4,
    maxLcom: Int = 2,
    minCloneSize: Int = 30
)

object Thresholds:
  /** Reads `maxCyclomatic=10` style options; unknown or malformed ones are ignored. */
  def fromOptions(options: List[String]): Thresholds =
    def get(name: String, default: Int): Int =
      options.collectFirst { case o if o.startsWith(name + "=") => o.drop(name.length + 1).trim.toIntOption }.flatten
        .filter(_ > 0).getOrElse(default)
    val d = Thresholds()
    Thresholds(
      get("maxCyclomatic", d.maxCyclomatic),
      get("maxCognitive", d.maxCognitive),
      get("maxMethodLines", d.maxMethodLines),
      get("maxParams", d.maxParams),
      get("maxNesting", d.maxNesting),
      get("maxLcom", d.maxLcom),
      get("minCloneSize", d.minCloneSize)
    )

/** Turns method and class measurements into findings with an explanation and a fix. */
object ClassMetrics:

  private def method(m: MethodFact, kind: String, value: Int, limit: Int, what: String, principle: String, fixes: List[String]) =
    val text = new StringBuilder
    text.append(s"Method `${m.sym}` $what: $value (limit $limit).\n")
    text.append(s"  Principle: $principle\n")
    text.append(s"  Fix with one of:\n")
    for f <- fixes do text.append(s"    - $f\n")
    text.append(s"  Done when `${m.sym}` measures $limit or less.")
    Finding(s"$kind ${m.sym}", value, m.file, m.line, text.toString)

  private val splitSteps = "Extract each step or branch body into a private method named after what it does."

  def findings(methods: List[MethodFact], classes: List[ClassFact], t: Thresholds): List[Finding] =
    val fromMethods = methods.flatMap { m =>
      List(
        Option.when(m.cyclomatic > t.maxCyclomatic)(
          method(
            m, "cyclomatic", m.cyclomatic, t.maxCyclomatic, "has too many independent paths (cyclomatic complexity)",
            "A method should be testable with a handful of cases (McCabe); every path needs its own test.",
            List(
              splitSteps,
              "Replace a chain of conditions on a type or tag with polymorphism or a lookup `Map`.",
              "Use guard clauses and pattern matching to remove else branches."
            )
          )
        ),
        Option.when(m.cognitive > t.maxCognitive)(
          method(
            m, "cognitive", m.cognitive, t.maxCognitive, "is too hard to follow (cognitive complexity)",
            "Code should be understandable top to bottom without holding many branches in your head; nesting costs most.",
            List(
              splitSteps,
              "Flatten nesting: handle the exceptional cases first, or use a for-comprehension over Option/Either.",
              "Name complex boolean conditions as `val`s or small predicates."
            )
          )
        ),
        Option.when(m.lines > t.maxMethodLines)(
          method(
            m, "lines", m.lines, t.maxMethodLines, "is too long (lines)",
            "A function should do one thing (Clean Code, ch. 3); long bodies mix several levels of abstraction.",
            List(splitSteps, "Move groups of statements that work on the same data into their own class.")
          )
        ),
        Option.when(m.params > t.maxParams)(
          method(
            m, "params", m.params, t.maxParams, "takes too many parameters (explicit parameters)",
            "Long parameter lists hide a missing concept and are easy to call wrongly (Clean Code, ch. 3).",
            List(
              "Group parameters that travel together into a case class and pass that.",
              "Check whether some parameters are state of the receiver and belong in a field or constructor."
            )
          )
        ),
        Option.when(m.nesting > t.maxNesting)(
          method(
            m, "nesting", m.nesting, t.maxNesting, "is nested too deeply (nesting depth)",
            "Each nesting level multiplies the context a reader must keep in mind.",
            List(
              "Extract the innermost block into a named method.",
              "Invert conditions into guard clauses or pattern matches to flatten the structure."
            )
          )
        )
      ).flatten
    }
    val fromClasses = classes.filter(_.clusters.size > t.maxLcom).map { c =>
      val groups = c.clusters.zipWithIndex.map { case (ms, i) => s"    ${i + 1}. ${ms.mkString(", ")}" }.mkString("\n")
      val n = c.clusters.size
      val text =
        s"Class `${c.sym}` has $n unrelated groups of methods (LCOM4 $n, limit ${t.maxLcom}).\n" +
          "  Principle: Single Responsibility. Methods in one class should work on shared state or call each other; " +
          "these groups share neither fields nor calls:\n" +
          s"$groups\n" +
          "  Fix with one of:\n" +
          s"    - Split into $n classes, one per group, moving with each group the fields it uses.\n" +
          "    - If a group uses no state at all, move it to an object or to the class whose data it works on.\n" +
          s"  Done when `${c.sym}` has at most ${t.maxLcom} groups."
      Finding(s"lcom ${c.sym}", n, c.file, c.line, text)
    }
    (fromMethods ++ fromClasses).groupBy(_.key).values.map(_.maxBy(_.value)).toList.sortBy(f => (f.file, f.line, f.key))
