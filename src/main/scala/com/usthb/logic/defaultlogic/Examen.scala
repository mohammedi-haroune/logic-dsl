package com.usthb.logic.defaultlogic

import com.usthb.logic.defaultlogic.Default._
import com.usthb.logic.propositional.Literals._
import com.usthb.logic.propositional._

import scala.collection.Set
import scala.language.implicitConversions

object Examen extends App {
  val w = Set[Formula](P -> (Q & R), R -> !S)

  val d = Set(
    Empty * P / P,
    R * !Q / !Q,
    S * T / T,
    P * V / V,
    Q * !V / !V,
    V * T / T
  )


  val delta = Theory(w, d)

  println(s"extentions = ${extentions(delta)}")

  def extentions(delta2: Theory) =
    delta2.extentions.mkString("[\n", "\n", "\n]")
}
