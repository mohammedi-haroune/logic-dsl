package com.usthb.logic.propositional

import com.usthb.logic.defaultlogic.Default._
import com.usthb.logic.defaultlogic.Theory
import com.usthb.logic.propositional.Formula._
import com.usthb.logic.propositional.Literals._

import scala.collection.Set
import scala.language.implicitConversions

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

  e.write("test2.cnf")

  val f2 = (P & True) -> (P & True) & Q | False

  println(s"shorthand of f = ${f2.shorthand}")

  /*val world1: Set[Formula] = Set(C -> D, (A & B) -> E, E | D, D -> F)
  val defaults1 = Set(
    ((E | F) * (A & F)) / (A & F),
    A * B / B,
    ((A & E) * C) / C,
    Empty * !E / !E
  )*/

  /*  val world2: Set[Formula] = Set(A -> (B & C), !T | A, D -> T, D, F -> E)

  val defaults2 = Set(
    ((B & D) * F) / F,
    E * P / P,
    F * !P / !P
  )

  val delta2 = Theory(world2, defaults2)*/

  val w: Set[Formula] = Set(A -> (B & C), !T | A, D -> T, D, F -> E)

  val d = Set(
    F * !P / !P,
    E * P / P,
    ((B & D) * F) / F
  )

  val delta = Theory(w, d)

  println(s"extentions = ${extentions(delta)}")

  def extentions(delta2: Theory) =
    delta2.extentions.mkString("[\n", "\n", "\n]")
}
