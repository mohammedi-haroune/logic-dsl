package com.usthb.logic.sematicnet

import scala.collection.mutable
import scala.collection.mutable.Set
import scala.util.Random

case class Marker(name: Symbol)(implicit net: SemanticNet) {
  net.addMarker(this)
  val color = Marker.getColor
  val open = Set.empty[Node]
  val history = mutable.Map[Node, Int]()
  var step = 0
  val closed: Set[Node] = mutable.Set.empty[Node]
  def isMarked(node: Node): Boolean = (open union closed).contains(node)

  def init(node: Node): Unit = {
    open += node
    history += node -> step
  }

  def expand(): Unit = {
    step = step + 1
    open
      .foreach { marked =>
        open -= marked
        if (!closed.contains(marked)) {
          closed += marked
          if (!history.contains(marked)) history += marked -> step

          marked.relations.foreach {
            case Is(specific, general) if general == marked =>
              open += specific
            case Instance(instance, concept) if concept == marked =>
              open += instance
            case _ =>
          }
        }
      }
  }

  def expandAll(): Unit = while (open.nonEmpty) expand()
}

object Marker {
  private val allColors = mutable.ArrayBuffer("green", "blue", "red")
  private var colors = mutable.ArrayBuffer("green", "blue", "red")
  def getColor = {
    val i = new Random().nextInt(colors.size)
    colors.remove(i)
  }

  def initialize() = colors = allColors.clone()
}
