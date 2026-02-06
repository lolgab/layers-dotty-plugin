package layers.plugin

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.transform.Pickler

class LayersPlugin extends StandardPlugin:
  val name: String = "layers"
  override val description: String = "Enforces package dependencies via @dependsOn annotations on the layer object"

  override def initialize(options: List[String])(using Context): List[PluginPhase] =
    (new AnnotationLayersPhase) :: Nil
