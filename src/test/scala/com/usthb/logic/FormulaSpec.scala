package com.usthb.logic
import org.scalatest._
import com.usthb.logic.Literals._
import org.scalatest.prop.TableDrivenPropertyChecks

class FormulaSpec
    extends PropSpec
    with Matchers
    with TableDrivenPropertyChecks {

  property("a well formed formula should be coverted to CNF propoerly") {
    val examples = Table(
      ("formula", "cnf"),
      (P, P),
      (!P, !P),
      (P | Q, P | Q),
      (P -> Q, !P | Q),
      (P -> (Q & R), (!P | Q) & (!P | R))
    )

    forAll(examples) { (f, cnf) =>
      f.toCNF shouldEqual cnf
    }
  }

  property("a well formed formula should be evaluated propoerly") {
    val examples = Table(
      ("formula", "evaluations", "value"),
      (P, Array(P := true), true),
      (P, Array(P := false), false),
      (!P, Array(P := false), true),
      (!P, Array(P := true), false),
      (P | Q, Array(P := true, Q := false), true),
      (P | Q, Array(P := false, Q := true), true),
      (P | Q, Array(P := true, Q := true), true),
      (P | Q, Array(P := false, Q := false), false),
      (P & Q, Array(P := true, Q := false), false),
      (P & Q, Array(P := false, Q := true), false),
      (P & Q, Array(P := true, Q := true), true),
      (P & Q, Array(P := false, Q := false), false),
      (P -> Q, Array(P := true, Q := false), false),
      (P -> Q, Array(P := false, Q := true), true),
      (P -> Q, Array(P := true, Q := true), true),
      (P -> Q, Array(P := false, Q := false), true),
      (P <-> Q, Array(P := true, Q := false), false),
      (P <-> Q, Array(P := false, Q := true), false),
      (P <-> Q, Array(P := true, Q := true), true),
      (P <-> Q, Array(P := false, Q := false), true)
    )

    forAll(examples) { (f, e, v) =>
      withValues(e: _*) eval f shouldEqual v
    }
  }
}
