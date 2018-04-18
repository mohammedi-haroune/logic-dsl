package com.usthb.logic

import com.usthb.logic.Formula.th
import com.usthb.logic.Literals._

import scala.collection.Set

object Main extends App{
  val f = P -> (Q & R) -> T
  println(s"the cnf form of $f is ${f.toCNF}")

  val e = Set(P, Q, P -> R, (P & Q) -> V)
  println(s"the theory of ${e.mkString(",")} is ${th(e).mkString(",")}")

  val world = Set(A)
  val defaults = Array(
    Default(A, C, B),
    Default(A, !B, !C)
  )

  val delta = Theory(world, defaults)
  println(s"extentions = ${delta.extentions}")
}
