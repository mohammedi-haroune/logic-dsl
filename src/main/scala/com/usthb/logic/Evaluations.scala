package com.usthb.logic

case class Evaluated(f: Formula, v: Boolean)

object withValues {
  def apply(evaluations: Evaluated*) = new FormulaEvaluator(evaluations: _*)
}

class FormulaEvaluator(evaluations: Evaluated*) {
  def eval(f: Formula): Boolean = evaluations.find(_.f == f) match {
    case Some(v) => v.v
    case None =>
      f match {
        case True              => true
        case False             => false
        case Negation(formula) => !eval(formula)
        case Implies(l, r)     => !(eval(l) && !eval(r))
        case Equivalent(l, r)  => eval(Implies(l, r)) && eval(Implies(r, l))
        case Or(l, r)          => eval(l) || eval(r)
        case And(l, r)         => eval(l) && eval(r)
        case _                 => throw new Exception(s"can't find evaluation for $f")
      }
  }
}
