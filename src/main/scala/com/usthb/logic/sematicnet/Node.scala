package com.usthb.logic.sematicnet

import scala.collection.mutable.Set
import scala.language.implicitConversions
import Node.symbole2Node

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.sys.process._
import scala.util.Random

trait Node {
  val name: Symbol
  val relations: Set[Relation]
  var isClass: Boolean = false
  def addRelation(r: Relation): Unit = relations += r

  def ->(name: Symbol): ConstructionReation = ConstructionReation(this, name)

  def canEqual(other: Any): Boolean = other.isInstanceOf[Node]

  override def equals(other: Any): Boolean = other match {
    case that: Node => (that canEqual this) && name == that.name
    case _          => false
  }

  def is(node: Node)(implicit net: SemanticNet): Unit =
    net.addRelation(Is(this, node))

  def instanceOf(node: Node)(implicit net: SemanticNet): Unit =
    net.addRelation(Instance(this, node))

  override def toString: String = name.name

  def getAllMarkers(implicit net: SemanticNet): mutable.Set[Marker] =
    net.markers.filter(_.isMarked(this))

  private def allMarkersString(implicit net: SemanticNet) =
    getAllMarkers
      .map(m =>
        s"<font color='${m.color}'> ${m.name.name}(${m.history(this)})</font>")
      .mkString(",")

  /*private val v1 = <font color='red'>${getAllMarkers
    .map(m => s"${m.name.name}(${m.history(this)})")
    .mkString(",")}</font>"*/

  def label(implicit net: SemanticNet) =
    s"label=<${name.name}<br /> $allMarkersString>"

  def graph(implicit net: SemanticNet): String =
    s"${name.name}" + "[" + (if (getAllMarkers.nonEmpty) label else "") + (if (isClass)
                                                                             " shape=rectangle"
                                                                           else
                                                                             "") + "]"
}

object Node {
  def apply(name1: Symbol)(implicit net: SemanticNet): Node = {
    net.getOrCreate(name1)
  }

  implicit def symbole2Node(s: Symbol)(implicit net: SemanticNet): Node =
    Node(s)
}

case class ConstructionReation(node1: Node, name: Symbol) {
  def ->(node2: Node)(implicit net: SemanticNet): Relation =
    BinaryRelation(name, node1, node2)
}

case class Question(node1: Symbol, relation: Symbol, node2: Symbol)(
    implicit net: SemanticNet) {
  require(net.contains(node1))
  require(net.contains(node2))

  private def expand(open: Set[Node], closed: Set[Node]): Unit = {
    open
      .foreach { marked =>
        if (!closed.contains(marked)) {
          closed += marked
          open -= marked
          marked.relations.foreach {
            case Is(specific, general) if general == marked =>
              open += specific
            case Instance(instance, concept) if concept == marked =>
              open += instance
          }
        }
      }
  }

  def solve: Set[(Node, Node)] = {

    val m1 = Marker('m1)
    val m2 = Marker('m2)

    m1.init(node1)
    m2.init(node2)

    m1.expandAll()
    m2.expandAll()
    val solutions =
      for {
        e1 <- m1.closed
        e2 <- m2.closed
        if net.relations.exists(
          r =>
            r.name == relation && r.arguments.contains(e1) && r.arguments
              .contains(e2))
      } yield (e1, e2)
    net.solutions ++= solutions
    solutions
  }
}
