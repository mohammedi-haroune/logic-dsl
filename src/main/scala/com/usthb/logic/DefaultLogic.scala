package com.usthb.logic

import scala.collection.{Set, mutable}
import com.usthb.logic.Formula._

/**
  * Implementaion for default in defaults logic, refer to default logic lecture for more information
  */
case class Default(prerequis: Formula, consequence: Formula, justificatifs: FormulaSet) {
  def isApplicable(world: FormulaSet): Boolean = world.contains(prerequis)
  def isUsable(world: FormulaSet): Boolean = justificatifs.map(Negation.apply).find(world.contains) match {
    case Some(_) => false
    case None => true
  }
}
object Default {
  def apply(prerequis: Formula, consequence: Formula, justificatif: Formula): Default = new Default(prerequis, consequence, Set(justificatif))
}

/**
  * Implemention for theory in defaults logic, refer to default logic lecture for more information
  */
case class Theory(world: FormulaSet, defaults: Array[Default]) {

  /**
    * Implemention for extention in defaults logic, refer to default logic lecture for more information
    */
  def extention(order: Seq[Default]): FormulaSet = {
    require(order.lengthCompare(defaults.length) <= 0, "order of defautls should contain for most number of defaults")
    val delta = mutable.Set.empty[Formula]
    delta ++= th(world)
    for (d <- order) {
      if (d.isApplicable(delta) && d.isUsable(delta)) {
        delta += d.consequence
        delta ++= th(delta)
      }
    }
    delta
  }

  /**
    * get all possible extentions for this Theory
    */
  def extentions: Set[FormulaSet] = defaults.permutations.map(x => extention(x)).toSet
}