package com.usthb.logic

import com.usthb.logic.Literals._

import scala.collection.Set
import scala.language.implicitConversions
import com.usthb.logic.Formula._


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

  e.writeDMACS("test.cnf")

  val world = Set(A)
  val defaults = Array(
    Default(A, C, B),
    Default(A, !B, !C)
  )

  val delta = Theory(world, defaults)

  println(s"extentions = ${delta.extentions}")


}
