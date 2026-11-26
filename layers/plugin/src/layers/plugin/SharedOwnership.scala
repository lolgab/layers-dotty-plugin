package layers.plugin

import scala.collection.mutable

/** A type in a shared package (`util`, `common`, ...) that is really owned by one slice.
  *
  * Shared code is a cost: every slice that uses it is coupled to every other that does. It earns that only when the
  * slices use it independently. When every class that uses the type also uses the same slice `X` (it works on `X`'s
  * data, or lives in `X`), the type belongs in `X`: moving it adds no dependency to anyone, and the shared package no
  * longer pretends the type is generic.
  *
  * Deliberately narrow:
  *   - the type must be used by classes of at least two slices, and by at least two classes
  *   - the owner must be the one slice all those classes depend on, and must not itself be a hub (moving code into a
  *     package that many depend on grows the kernel)
  *   - moving it must not make `X` depend on something that depends on `X`
  */
object SharedOwnership:

  private val minUsers = 2

  def findings(slices: Slices): List[Finding] =
    val facts = slices.facts
    val deps = facts.flatMap(_.classDeps)
    val decls = facts.flatMap(_.typeDecls).filter(d => Slices.isTopLevel(d) && !d.local).groupBy(Slices.topOf).view.mapValues(_.head).toMap
    // what each class depends on, as slices: its own and every one it references
    val domain = mutable.Map[String, mutable.Set[String]]()
    for d <- deps do
      val set = domain.getOrElseUpdate(d.fromTop, mutable.Set())
      if slices.isSlice(d.fromPkg) then set += d.fromPkg
      if slices.isSlice(d.toPkg) then set += d.toPkg
    val usersOf = deps.groupBy(_.toTop)
    val out = for
      (top, decl) <- decls.toList.sortBy(_._1)
      if slices.isShared(decl.pkg)
      users = usersOf.getOrElse(top, Nil).filter(d => slices.isSlice(d.fromPkg)).groupBy(_.fromTop).view.mapValues(_.head).toMap
      if users.size >= minUsers && users.values.map(_.fromPkg).toSet.size >= minUsers
      owner <- ownerOf(slices, top, users.keys.toList, domain.toMap, deps)
    yield
      val files = users.values.toList.sortBy(d => (d.fromPkg, d.fromTop))
      val sb = new StringBuilder
      sb.append(
        s"`$top` is in the shared package `${decl.pkg}` but belongs to slice `$owner`: all its ${users.size} users " +
          s"(${files.take(6).map(d => Slices.simple(d.fromTop)).mkString(", ")}${if files.size > 6 then ", ..." else ""}) " +
          s"live in or depend on `$owner`.\n"
      )
      sb.append(
        "  Principle: shared code couples every slice that uses it to every other. A type that only works together " +
          "with one slice's data is that slice's type, not a shared one.\n"
      )
      sb.append(s"  Users, by slice:\n")
      for (pkg, ds) <- files.groupBy(_.fromPkg).toList.sortBy(_._1) do
        sb.append(s"    $pkg: ${ds.map(d => s"${Slices.simple(d.fromTop)} (${d.file}:${d.line})").take(4).mkString(", ")}\n")
      sb.append(s"  Fix: move `${Slices.simple(top)}` (${decl.file}) into `$owner`. The users already depend on `$owner`, so no dependency is added.\n")
      sb.append("  Widening the type or adding a second unrelated user does not make it shared: it must be used without the owner's data.\n")
      sb.append(s"  Done when `$top` is no longer in `${decl.pkg}`.")
      Finding(s"shared-owned $top", users.size, decl.file, decl.line, sb.toString, List((decl.line, decl.line)))
    out

  /** The one slice all users depend on, if moving the type there is safe. */
  private def ownerOf(
      slices: Slices, top: String, users: List[String], domain: Map[String, mutable.Set[String]], deps: List[ClassDep]
  ): Option[String] =
    val common = users.map(u => domain.getOrElse(u, mutable.Set.empty[String]).toSet).reduce(_ intersect _)
    // the type's own uses of slices are part of what it would take along
    val own = deps.filter(_.fromTop == top).map(_.toPkg).filter(slices.isSlice).toSet
    common.toList match
      case List(x) if slices.dependents(x).size <= slices.options.maxFanIn && !own.exists(y => y != x && reaches(slices.graph, y, x)) =>
        Some(x)
      case _ => None

  /** `from` depends on `to`, directly or through other packages. */
  private[plugin] def reaches(graph: PackageGraph, from: String, to: String): Boolean =
    val seen = mutable.Set(from)
    val queue = mutable.Queue(from)
    var found = false
    while queue.nonEmpty && !found do
      for n <- graph.succ.getOrElse(queue.dequeue(), Nil) do
        if n == to then found = true
        else if seen.add(n) then queue.enqueue(n)
    found
