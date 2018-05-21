package com.usthb.logic.sematicnet

import scala.collection.mutable.Set

class SemanticNet {
  implicit val net: SemanticNet = this

  val nodes: Set[Node] = Set.empty[Node]

  val relations: Set[Relation] = Set.empty[Relation]
  val markers: Set[Marker] = Set.empty[Marker]
  def addRelation(relation: Relation): Unit = {
    relation.arguments.foreach(_.addRelation(relation))
    relations += relation
  }

  def addMarker(marker: Marker): markers.type = markers += marker

  def getOrCreate(symbol: Symbol): Node = {
    nodes.find(_.name == symbol) match {
      case Some(n) => n
      case None =>
        val n = new Node {
          override val name: Symbol = symbol
          override val relations: Set[Relation] = Set.empty[Relation]
        }
        nodes += n
        n
    }
  }

  override def toString = s"Relations: ${relations.mkString("[", ",", "]")}) \n Nodes: ${nodes.mkString("[", ",", "]")}"

  def graph: String =
    s"""
      |digraph {
      |rankdir=BT
      |${nodes.map(_.graph).mkString(";\n")}
      |${relations.map(_.graph).mkString(";\n")}
      |}
    """.stripMargin

  def contains(node: Node): Boolean = nodes.contains(node)
  def contains(symbol: Symbol): Boolean = nodes.contains(symbol)
}

object SemanticNet {
  def empty: SemanticNet = new SemanticNet
}