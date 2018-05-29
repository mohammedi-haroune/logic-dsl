package com.usthb.logic
import com.usthb.logic.propositional.Formula.FormulaSet
import org.scalatest._
import com.usthb.logic.propositional.Literals._
import com.usthb.logic.propositional.{Formula, True, withValues}
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
      (P -> (Q & R), (!P | Q) & (!P | R)),
      (!(P & (P -> P)), (!P | P) & (!P | !P))
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


  property("a well formed formula should be simplified propoerly") {
    val examples = Table(
      ("formula", "simplification"),
      (P, P),
      (!P, !P),
      (P | Q, P | Q),
      (P & (P -> P), P),
      (P & P & P & P, P),
      (P & True, P)
    )

    forAll(examples) { (f, s) =>
      f.shorthand shouldEqual s
    }
  }

  property("isInfereV2 should infere properly") {
    val examples = Table[Formula, FormulaSet, Boolean](
      ("formula", "set", "is inferred from"),
      (P, Set(P), true),
      (!P, Set(!P), true),
      (P | Q, Set(P | Q), true),
      (P & (P -> P), Set(P), true)
    )

    forAll(examples) { (f, s, bool) =>
      (s |= f) shouldEqual bool
    }
  }
}
