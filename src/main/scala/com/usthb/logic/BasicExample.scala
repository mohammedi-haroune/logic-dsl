package com.usthb.logic

import com.usthb.logic.Literals._

import scala.collection.Set
import scala.language.implicitConversions
import com.usthb.logic.Formula._
import com.usthb.logic.Default._


object BasicExample extends App {
  val value =
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
  val defaults1 = Array(
    ((E | F) * (A & F)) / (A & F),
    A * B / B,
    ((A & E) * C) / C,
    Empty * !E / !E
  )

  val delta1 = Theory(world1, defaults1)

  println(s"extentions = \n${delta1.extentions.map{case (k, v) => (k -- world1, v)}.mkString("\n")}")

  val world = Set(A)
  val defaults = Array(
    (A * B) / C,
    (A * !C) / !B
  )

  val delta = Theory(world, defaults)

  println(s"extentions = ${delta.extentions}")
}
