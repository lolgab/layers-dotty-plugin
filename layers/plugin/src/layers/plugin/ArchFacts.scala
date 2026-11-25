package layers.plugin

/** A raw observation made by the architecture checks (domain purity, identity, god types, scattered transitions,
  * bloated handlers, rules by relationship), kept unevaluated like every other fact so options can change without
  * recompiling. One record per observation; what `fields` mean depends on `kind` (see [[ArchExtract]]).
  */
final case class ArchFact(kind: String, file: String, line: Int, fields: List[String]):
  def field(i: Int): String = fields.lift(i).getOrElse("")

  def list(i: Int): List[String] = ArchFact.items(field(i))

object ArchFact:
  private def clean(s: String): String = s.replace('\t', ' ').replace('\n', ' ').replace('\r', ' ')

  /** Joins items into one field; the separator never occurs inside an item. */
  def join(xs: Iterable[String]): String = xs.map(x => clean(x).replace(';', '_')).mkString(";")

  def items(s: String): List[String] = if s.isEmpty then Nil else s.split(";", -1).toList

  def encode(f: ArchFact): String =
    (List("arch", clean(f.kind), f.line.toString) ++ f.fields.map(clean)).mkString("\t")

  /** `parts` is the split line, starting with the `arch` tag. */
  def decode(source: String, parts: Array[String]): Option[ArchFact] = parts match
    case Array("arch", kind, line, rest*) if line.toIntOption.isDefined => Some(ArchFact(kind, source, line.toInt, rest.toList))
    case _                                                              => None

/** Options of the architecture checks. All are read from `-P:layers:<name>=<value>`; unknown or malformed ones are
  * ignored.
  *
  * @param domain
  *   packages that hold the domain; empty to infer them (see [[Domains]])
  * @param identityTypes
  *   simple names or qualified prefixes of the types that stand for the acting identity or request context; empty to
  *   use the vocabulary built in
  * @param pureExempt
  *   qualified names (prefixes) of classes or methods allowed to touch the clock, randomness or IO from the domain
  * @param layerKinds
  *   package to layer kind overrides (`persistence`, `rendering`, `web`, `integration`, `auth`, `domain`, `support`
  *   or `none`)
  */
final case class ArchConfig(
    domain: List[String] = Nil,
    identityTypes: List[String] = Nil,
    pureExempt: List[String] = Nil,
    layerKinds: Map[String, String] = Map.empty,
    godTypeMinPackages: Int = 3,
    godTypeMinFields: Int = 12,
    godTypeMaxOverlap: Int = 50,
    godServiceMinMethods: Int = 10,
    minTransitionSites: Int = 3,
    maxLayersPerMethod: Int = 4,
    layerMixMinCyclomatic: Int = 4,
    ruleByRelationship: Boolean = false,
    domainMinDependents: Int = 3
):
  /** The part of the configuration that changes what is recorded: facts written under another one are stale. */
  def extractionKey: String = identityTypes.sorted.mkString(",") + "|" + layerKinds.toList.sorted.mkString(",")

object ArchConfig:
  def fromOptions(options: List[String]): ArchConfig =
    def raw(name: String): Option[String] = options.collectFirst { case o if o.startsWith(name + "=") => o.drop(name.length + 1).trim }
    def strings(name: String): List[String] = raw(name).toList.flatMap(_.split(',').toList.map(_.trim).filter(_.nonEmpty))
    def int(name: String, default: Int): Int = raw(name).flatMap(_.toIntOption).filter(_ > 0).getOrElse(default)
    val d = ArchConfig()
    ArchConfig(
      domain = strings("domain"),
      identityTypes = strings("identityTypes"),
      pureExempt = strings("pureExempt"),
      layerKinds = strings("layerKinds").flatMap(_.split(':').toList match
        case pkg :: kind :: Nil => Some(pkg -> kind)
        case _                  => None
      ).toMap,
      godTypeMinPackages = int("godTypeMinPackages", d.godTypeMinPackages),
      godTypeMinFields = int("godTypeMinFields", d.godTypeMinFields),
      godTypeMaxOverlap = int("godTypeMaxOverlap", d.godTypeMaxOverlap),
      godServiceMinMethods = int("godServiceMinMethods", d.godServiceMinMethods),
      minTransitionSites = int("minTransitionSites", d.minTransitionSites),
      maxLayersPerMethod = int("maxLayersPerMethod", d.maxLayersPerMethod),
      layerMixMinCyclomatic = int("layerMixMinCyclomatic", d.layerMixMinCyclomatic),
      ruleByRelationship = options.contains("ruleByRelationship"),
      domainMinDependents = int("domainMinDependents", d.domainMinDependents)
    )

/** What kind of code a package is, inferred from its name and, for libraries, from what they are known for. Kinds are
  * `persistence`, `rendering`, `web`, `integration` (external services, network) and `auth`; `support` marks
  * serialisation and utility packages, which are neither domain nor infrastructure.
  */
object PackageKinds:
  private def under(pkg: String, prefixes: Iterable[String]): Boolean = prefixes.exists(p => pkg == p || pkg.startsWith(p + "."))

  private val persistenceLibs = List(
    "com.augustnagro", "slick", "doobie", "anorm", "scalikejdbc", "io.getquill", "org.sqlite", "org.postgresql", "org.h2",
    "com.mysql", "com.zaxxer", "org.hibernate", "jakarta.persistence", "javax.persistence", "java.sql", "javax.sql", "org.jooq",
    "skunk", "org.mongodb", "com.mongodb", "redis.clients", "org.flywaydb"
  )
  private val webLibs = List(
    "ba.sake", "io.undertow", "javax.servlet", "jakarta.servlet", "akka.http", "org.apache.pekko.http", "org.http4s", "cask",
    "play.api.mvc", "zio.http", "org.eclipse.jetty", "sttp.tapir", "sttp.model", "io.netty", "io.vertx", "org.scalatra",
    "com.sun.net.httpserver", "org.glassfish.jersey"
  )
  private val renderingLibs = List(
    "scalatags", "play.twirl", "org.xhtmlrenderer", "com.lowagie", "org.thymeleaf", "com.itextpdf", "org.apache.poi", "laminar",
    "com.raquo"
  )
  private val integrationLibs = List(
    "requests", "sttp.client3", "sttp.client4", "sttp.client", "scalaj.http", "okhttp3", "org.apache.http", "org.apache.hc",
    "jakarta.xml.ws", "javax.xml.ws", "com.sun.xml.ws", "com.stripe", "org.eclipse.angus", "jakarta.mail", "javax.mail",
    "io.sentry", "com.twilio", "software.amazon", "com.amazonaws", "com.google.cloud", "org.apache.kafka", "com.rabbitmq",
    "com.slack", "org.telegram", "nl.martijndwars", "com.sendgrid", "com.mailjet"
  )
  private val authLibs = List("pdi.jwt", "org.pac4j", "javax.security", "jakarta.security", "io.undertow.security", "com.auth0", "org.springframework.security", "de.mkammerer")

  private val persistenceNames = Set("database", "databases", "db", "persistence", "repository", "repositories", "repo", "repos", "dao", "daos", "storage", "sql", "jdbc", "store", "stores", "orm")
  private val renderingNames = Set("rendering", "render", "view", "views", "html", "ui", "templates", "template", "pages", "presentation", "components")
  private val webNames = Set("routes", "route", "routing", "router", "http", "web", "controller", "controllers", "rest", "servlet", "endpoint", "endpoints", "api")
  private val integrationNames = Set(
    "integration", "integrations", "gateway", "gateways", "client", "clients", "adapter", "adapters", "external", "connector", "connectors",
    "sian", "stripe", "email", "mail", "telegram", "webpush", "sms", "payment", "payments", "notification", "notifications", "slack",
    "s3", "kafka", "whatsapp", "sdi", "paypal", "twilio", "smtp", "push"
  )
  private val authNames = Set("auth", "authn", "authz", "security", "identity", "login", "authentication", "authorization")
  private val supportNames = Set("util", "utils", "common", "shared", "support", "helper", "helpers", "lib", "json", "serialization", "codec", "codecs", "ids")
  val domainNames: Set[String] = Set("model", "models", "domain", "entity", "entities", "core")

  /** The kind of a library package (or of the standard library's own infrastructure). */
  def libraryKind(pkg: String): Option[String] =
    if under(pkg, persistenceLibs) then Some("persistence")
    else if under(pkg, webLibs) then Some("web")
    else if under(pkg, renderingLibs) then Some("rendering")
    else if under(pkg, integrationLibs) then Some("integration")
    else if under(pkg, authLibs) then Some("auth")
    else None

  /** The kind a package's own name gives it: the last segment first, so `app.database.sql` is `persistence`. */
  def nameKind(pkg: String): Option[String] =
    pkg.split('.').toList.reverse.view.flatMap { s =>
      val n = s.toLowerCase
      if persistenceNames(n) then Some("persistence")
      else if renderingNames(n) then Some("rendering")
      else if webNames(n) then Some("web")
      else if integrationNames(n) then Some("integration")
      else if authNames(n) then Some("auth")
      else if supportNames(n) then Some("support")
      else None
    }.headOption

  def isDomainName(pkg: String): Boolean = pkg.split('.').exists(s => domainNames(s.toLowerCase))

  /** Overrides first (the longest matching package prefix), then the name, then the library. */
  def kind(pkg: String, cfg: ArchConfig, internal: Boolean): Option[String] =
    val over = cfg.layerKinds.filter { case (p, _) => pkg == p || pkg.startsWith(p + ".") }.toList.sortBy(-_._1.length).headOption.map(_._2)
    over match
      case Some("none") => None
      case Some(k)      => Some(k)
      case None         => if internal then nameKind(pkg) else libraryKind(pkg)

  /** The infrastructure kinds: code of these kinds does I/O or talks to a framework, so the domain must not use it. */
  val infrastructure: Set[String] = Set("persistence", "rendering", "web", "integration")

/** Which packages hold the domain. */
object Domains:

  /** Packages of `facts` that are domain packages. With `domain=` option the named packages and their subpackages.
    * Otherwise a package is domain when
    *  - its own name does not make it infrastructure, a support package (`util`, `json`) or an identity package,
    *  - it depends on no infrastructure package of the project, and
    *  - it is called `model` or `domain`, or at least `domainMinDependents` other packages depend on it: a core the rest
    *    is built on, not a feature of its own.
    */
  def infer(cfg: ArchConfig, facts: Iterable[FileFacts], graph: PackageGraph): Set[String] =
    val packages = facts.flatMap(_.packages).toSet
    if cfg.domain.nonEmpty then packages.filter(p => cfg.domain.exists(d => p == d || p.startsWith(d + ".")))
    else
      def kindOf(p: String) = PackageKinds.kind(p, cfg, internal = true)
      packages.filter { p =>
        kindOf(p).isEmpty &&
        !graph.succ.getOrElse(p, Nil).exists(d => kindOf(d).exists(k => PackageKinds.infrastructure(k) || k == "auth")) &&
        facts.exists(f => f.types.get(p).exists(_._1 > 0)) &&
        (PackageKinds.isDomainName(p) || graph.afferent(p) >= cfg.domainMinDependents)
      }

/** Reads the architecture facts back as typed rows. */
object ArchRows:
  def of(facts: Iterable[FileFacts], kind: String): List[ArchFact] = facts.iterator.flatMap(_.arch).filter(_.kind == kind).toList
