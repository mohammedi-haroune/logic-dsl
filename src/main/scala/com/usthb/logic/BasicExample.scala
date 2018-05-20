package com.usthb.logic

import com.usthb.logic.Literals._

import scala.collection.Set
import scala.language.implicitConversions
import com.usthb.logic.Formula._
import com.usthb.logic.Default._


object BasicExample extends App {
/*  val value =
    withValues(
      P := true,
      Q := true,
    ) eval (P ⊃ Q)

  println(s"eval = ${value}")

  val e = Set(P, Q, P ⊃ R, (P ∧ Q) ⊃ V)

  println(s"clause = ${e.toClause}")

  println(s"DMACS = ${e.toDMACS}")

  e.write("test.cnf")

  val f2 = (P & True) -> (P & True) & Q | False

  println(s"shorthand of f2 = ${f2.shorthand}")

  val world1: Set[Formula] = Set(C -> D, (A & B) -> E, E | D, D -> F)
  val defaults1 = Set(
    ((E | F) * (A & F)) / (A & F),
    A * B / B,
    ((A & E) * C) / C,
    Empty * !E / !E
  )*/


  val world2: Set[Formula] = Set(A -> (B & C), !T | A, D -> T, D, F -> E)

  val defaults2 = Set(
    ((B & D) * F) / F,
    E * P / P,
    F * !P / !P
  )

  val delta2 = Theory(world2, defaults2)

  /*val world1: Set[Formula] = Set()

  val defaults1 = Set(
    Empty * B / !B,
    Empty * !B / B
  )

  val delta = Theory(world1, defaults1)
  */

  println(s"extentions = ${delta2.extentions.mkString("[\n", "\n", "\n]")}")
}
