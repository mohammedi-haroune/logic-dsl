package com.usthb.logic

import scala.collection.{Set, mutable}
import com.usthb.logic.Formula._

import scala.language.postfixOps

/** A Default in logic default has three parts : prerequis, justificatifs and consequence
  *
  *   1. a default is applicable for a world W when the ''`World |= prerequis`''
  *   2. an applicable default is usable when for all ''`j`'' from justificatifs ''`World |= !j`'' is false
  *
  * We highly recommend you to use the provided dsl syntax to define Defalut (you should import com.usthb.logic.Default._)
  *
  * @example `Default(P, Set(Q), Q)` is defined using `(P * Q) / Q`
  *
  * @param prerequis the prerequis formulas
  * @param justificatifs a set of formulas
  * @param consequence the consequence formulas
  * @note refer to default logic lecture for more information
  */
case class Default(prerequis: Formula,
                   justificatifs: FormulaSet,
                   consequence: Formula) {
  val num: Int = Default.num
  Default.num = Default.num + 1
  def isApplicable(world: FormulaSet): Boolean = if (prerequis == Empty) true else world |= prerequis
  def isUsable(world: FormulaSet): Boolean =
    justificatifs.map(Negation.apply).find(world |= _) match {
      case Some(_) => false
      case None    => true
    }

  override def toString: String = s"d${num}"
}
object Default {
  var num = 1
  def apply(prerequis: Formula,
            justificatif: Formula,
            consequence: Formula): Default =
    new Default(prerequis, Set(justificatif), consequence)
  implicit class FormulaOps(f: Formula) {
    def *(formules: Formula*): Justificatifs =
      new Justificatifs(f, formules.toSet)
  }

  class Justificatifs(f: Formula, j: FormulaSet) {
    def /(c: Formula) = Default(f, j, c)
  }
}

/**
  * Implemention for theory in defaults logic, refer to default logic lecture for more information
  */
case class Theory(world: FormulaSet, defaults: Array[Default]) {

  /**
    * Implemention for extention in defaults logic, refer to default logic lecture for more information
    * @param order a sequence of [[com.usthb.logic.Default]] that will be executing in the given order
    * @return the extension found by using the given defaults in order and a list of generators defaults
    */
  private def extention(order: Seq[Default]): (FormulaSet, List[Default]) = {
    require(order.lengthCompare(defaults.length) <= 0,
            "order of defautls should contain for most number of defaults")
    val delta = mutable.Set.empty[Formula]
    var generators = List.empty[Default]
    delta ++= world
    for (d <- order) {
      if (d.isApplicable(delta) && d.isUsable(delta)) {
        delta += d.consequence
        generators = d :: generators
      }
    }
    (delta, generators.reverse)
  }

  /**
    * get all possible extentions for this Theory
    * @return all extensions associated with their generators
    */
  def extentions: Map[FormulaSet, Predef.Set[String]] =
    defaults.permutations.map(x => extention(x)).toList.groupBy(_._1).mapValues(_.map(_._2.mkString("<")).toSet)
}
