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



  val world = Set(A)
  val defaults = Array(
    (A * B) / C,
    (A * !C) / !B
  )

  val delta = Theory(world, defaults)

  println(s"extentions = ${delta.extentions}")

  println((P & (P -> P)).shorthand)
}
