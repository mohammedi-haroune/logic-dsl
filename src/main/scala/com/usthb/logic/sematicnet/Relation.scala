package com.usthb.logic.sematicnet

trait Relation {
  def name: Symbol
  def arity: Int
  def argumentsDef: Array[String]
  def arguments: Array[Node]
  def isTransitive: Boolean = false

  /*require(arguments.length == arity,
          s"wrong number of arguments ${arguments.length}, expected $arity")
  require(argumentsDef.length == arity,
          s"wrong number of arguments $arguments.length, expected $arity")*/

  override def toString =
    s"$name(${argumentsDef
      .zip(arguments)
      .map { case (arg, value) => s"$arg=$value" }
      .mkString(",")})"

  def graph(implicit net: SemanticNet): String = "not supported yet"
}

object Relation {
  def apply(name1: Symbol, arguments1: Node*)(
    implicit net: SemanticNet): Relation = {
    val relation = new Relation {
      override def name: Symbol = name1
      override def arity: Int = arguments1.length
      override def argumentsDef: Array[String] =
        (1 to arity).map(arg => s"arg$arg").toArray
      override def arguments: Array[Node] = arguments1.toArray
    }
    net.addRelation(relation)
    relation
  }

  def apply(name1: Symbol,
            argumentsDef1: Array[String],
            arguments1: Array[Node])(implicit net: SemanticNet): Relation = {
    val relation = new Relation {
      override def name: Symbol = name1
      override def arity: Int = arguments1.length
      override def argumentsDef: Array[String] = argumentsDef1
      override def arguments: Array[Node] = arguments1
    }

    net.addRelation(relation)
    relation
  }
}

class BinaryRelation(override val name: Symbol, val arg1: Node, val arg2: Node) extends Relation {
  override def arity: Int = 2
  override def argumentsDef: Array[String] = Array("arg1", "arg2")
  override def arguments: Array[Node] = Array(arg1, arg2)
  override def graph(implicit net: SemanticNet): String = s"$arg1 -> $arg2 [label=${name.name} ${
    if (net.solutions.contains((arg1, arg2))) "color = red" else ""} ]"

  override def toString: String = s"$arg1 ${name.name} $arg2"
}

object BinaryRelation {
  def apply(name: Symbol, arg1: Node, arg2: Node)(implicit net: SemanticNet): BinaryRelation = {
    val relation = new BinaryRelation(name, arg1, arg2)
    net.addRelation(relation)
    relation
  }
}

case class Is(override val arg1: Node, override val arg2: Node) extends BinaryRelation('is_a, arg1, arg2) {
  override def isTransitive: Boolean = true
  arg1.isClass = true
  arg2.isClass = true
}

case class Instance(override val arg1: Node, override val arg2: Node) extends BinaryRelation('instance, arg1, arg2) {
  override def isTransitive: Boolean = true
  arg2.isClass = true
}
