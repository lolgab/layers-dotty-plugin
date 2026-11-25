package layers.plugin

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

/** Developer tool: runs the architecture checks on the facts a compile left in `<out>/META-INF/layers-facts`, with
  * other options, without recompiling. Usage: `ArchReplay <facts dir> [option ...]`.
  */
object ArchReplay:
  def main(args: Array[String]): Unit =
    val dir = Path.of(args.head)
    val options = args.tail.toList
    val facts = Files.list(dir).iterator.asScala.filter(_.getFileName.toString.endsWith(".tsv")).toList.flatMap(f => FileFacts.decode(Files.readString(f)))
    val cfg = ArchConfig.fromOptions(options)
    val graph = PackageGraph.build(facts)
    println(s"domain: ${Domains.infer(cfg, facts, graph).toList.sorted.mkString(", ")}")
    for f <- ArchChecks.findings(facts, graph, cfg) do
      if options.contains("full") then println(f.message + "\n") else println(s"${f.key} = ${f.value}")
