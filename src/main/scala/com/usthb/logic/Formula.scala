package com.usthb.logic

import java.io.File
import java.nio.file.{Files, Paths}

import com.usthb.logic.Formula.FormulaSet

import scala.collection.JavaConverters._
import scala.collection.{Set, mutable}
import sys.process._
import Formula._

import scala.language.implicitConversions

/**
  * Abstract a well formed formula of propositional logic which are obtained by using the construction rules below:
  *   -  An atomic proposition f is a well-formed formula.
  *   -  If f is a well-formed formula, then so is f.
  *   -  If f1 and f2 are well-formed formulas, then so are f1 ∨ f2, f1 ∧ f2, f1 ⊃ f2, and f1 <=> f2.
  *   -  If f is a well-formed formula, then so is (f).
  */
sealed trait Formula {

  /**
    * Logical or
    * @return a new formula composed of this ∨ f
    */
  def ∨(f: Formula): Or = or(f)

  /**
    * alias for ∨ operator
    */
  def |(f: Formula): Or = or(f)

  /**
    * alias for ∨ operator
    */
  def or(f: Formula): Or = Or(this, f)

  /**
    * Logical and
    * @return a new f composed for this ∧ f
    */
  def ∧(f: Formula): And = and(f)

  /**
    * alias for ∧ operator
    */
  def &(f: Formula): And = and(f)

  /**
    * alias for ∧ operator
    */
  def and(f: Formula): And = And(this, f)

  /**
    * Logical implies
    * @return a new f composed for this ⊃ f
    */
  def ⊃(f: Formula): Implies = implies(f)

  /**
    * alias for ⊃ operator
    */
  def ->(f: Formula): Implies = implies(f)

  /**
    * alias for ⊃ operator
    */
  def implies(f: Formula): Implies = Implies(this, f)

  /**
    * Logical equivalent
    * @return a new f composed for this <-> f
    */
  def <->(f: Formula): Equivalent = equivalent(f)

  /**
    * alias for <-> operator
    */
  def <=>(f: Formula): Equivalent = equivalent(f)

  /**
    * alias for <-> operator
    */
  def equivalent(f: Formula) = Equivalent(this, f)

  /**
    * Logical not
    * @return a new f composed for !f
    */
  def unary_! : Formula = Negation(this)

  /**
    * alias for ! operator
    */
  def unary_not: Formula = !this

  def :=(v: Boolean) = Evaluated(this, v)

  /**
    * Convert this well formed formula to conjuctive normale form [[https://en.wikipedia.org/wiki/Conjunctive_normal_form]]
    */
  def toCNF: Formula = {
    this match {
      case Literal(_) | Negation(Literal(_)) => this
      case Negation(Or(l, r))                => And(Negation(l).toCNF, Negation(r).toCNF).toCNF
      case Negation(And(l, r))               => Or(Negation(l).toCNF, Negation(r).toCNF).toCNF
      case Negation(f)                       => Negation(f.toCNF).toCNF
      case Implies(l, r)                     => Or(Negation(l).toCNF, r.toCNF).toCNF
      case Equivalent(l, r) =>
        And(Implies(l, r).toCNF, Implies(r, l).toCNF).toCNF
      case Or(l, And(l1, r1)) => And(Or(l, l1).toCNF, Or(l, r1).toCNF).toCNF
      case Or(And(l1, r1), l) => And(Or(l1, l).toCNF, Or(r1, l).toCNF).toCNF
      case Or(l, r)           => Or(l.toCNF, r.toCNF)
      case And(l, r)          => And(l.toCNF, r.toCNF)
      case _ =>
        throw new Exception(s"not yet supproted to convert to CNF $this")
    }
  }

  def toClause: FormulaSet = this.toCNF match {
    case And(l, r) => l.toClause union r.toClause
    case cnf       => Set(cnf)
  }

  def toDMACS: Set[Set[Int]] = {
    toClause.map(f => f.encode)
  }

  def encode: Set[Int] = this match {
    case l: Literal           => Set(l.num)
    case Negation(l: Literal) => Set(-l.num)
    case Or(l, r)             => l.encode union r.encode
    case _                    => throw new Exception(s"cannot encode not a clause $this")
  }

  override def toString: String = {
    this match {
      case True        => "true"
      case False       => "false"
      case Literal(x)  => x.name
      case Negation(x) => "(" + "¬ " + x.toString + ")"
      case f: BinaryFormula =>
        "(" + f.l.toString + " " + f.op.name + " " + f.r.toString + ")"
    }
  }

  def shorthand: Formula = this match {
    case _: Literal | Negation(Literal(_)) | True | False => this
    case And(f, True)                                     => f.shorthand
    case And(True, f)                                     => f.shorthand
    case And(f, False)                                    => False
    case And(False, f)                                    => False
    case Or(f, True)                                      => True
    case Or(True, f)                                      => True
    case Or(f, False)                                     => f.shorthand
    case Or(False, f)                                     => f.shorthand
    case Implies(l, r) if l == r                          => True
    case Equivalent(l, r) if l == r                       => True
    case And(l, r) if l == r                              => l.shorthand
    case Or(l, r) if l == r                               => l.shorthand
    case And(l, r) if r |= l                              => r.shorthand
    case And(l, r) if l |= r                              => l.shorthand
    case And(l, r)                                        => And(l.shorthand, r.shorthand)
    case Or(l, r)                                         => Or(l.shorthand, r.shorthand)
  }
}

object Formula {

  type FormulaSet = Set[Formula]

  implicit def formula2Set(f: Formula): FormulaSetOps = FormulaSetOps(Set(f))

  implicit class FormulaSetOps(set: FormulaSet) {
    def toClause: Set[Formula] = set.flatMap(_.toClause)
    def toDMACS: Set[Set[Int]] =
      set.map(_.toCNF).map(_.toDMACS).reduce(_ union _)
    def write(path: String): Unit = Formula.write(set, path)

    def |=(f: Formula): Boolean = isInferedV2(f, set)
  }

  def write(set: FormulaSet, path: String): Unit = {
    val writer = Files.newBufferedWriter(Paths.get(path))
    val dmacs = set.toDMACS
    val vars = dmacs.flatten.map(_.abs).toSet
    var mapping = vars.zipWithIndex.map(v => (v._1, v._2 + 1)).toMap
    mapping ++= mapping.map(c => (-c._1, -c._2))

    writer.write(s"p cnf ${vars.size} ${dmacs.size}\n")
    dmacs.map(_.map(mapping).mkString(" ") + " 0\n").foreach(writer.write)
    writer.close()
  }

  /**
    * an implicit conversion from [[Symbol]] to [[Formula]]
    * @param symbol the symble to covert
    * @return a new formula (a Literal) with name == [[Symbol.name]]
    */
  implicit def symbole2Formule(symbol: Symbol): Formula = Literal(symbol)

  /**
    * Check if the a formula can be infered from a set of formulas
    * @param f the formula to be checked
    * @param set the set of formulas to check with
    * @return true if the given formula can be infered from the given set. false otherwise
    */
  def isInfered(f: Formula, set: FormulaSet): Boolean =
    if (set.contains(f)) true
    else
      f match {
        case l: Literal => set.contains(l)
        case And(l, r)  => isInfered(l, set) && isInfered(r, set)
        case Or(l, r)   => isInfered(l, set) || isInfered(r, set)
        case _          => throw new Exception(s"not yet supported isInfered for $f")
      }

  def isInferedV2(f: Formula, set: FormulaSet): Boolean = {
    val path = "test.cnf"
    (set + (!f)).write(path)
    val result = s"ubcsat -alg saps -i ${path} -solve".!!
    new File(path).delete()
    result.contains("No Solution found ")
  }

  def modusPonun(implication: Formula, left: Formula): FormulaSet =
    modusPonun(implication, Set(left))

  /**
    * Apply the modus ponun rule for the given formuals as follows R3(implication, lefts)
    * @param implication the formulat that contains the implication
    * @param lefts a set formulas
    * @return a set contains the right part of implication formula if (lefts set contains the left part). an empty set otherwise
    */
  def modusPonun(implication: Formula, lefts: FormulaSet): FormulaSet =
    implication match {
      case Implies(l, r) if lefts.contains(l) => Set(r)
      case _                                  => Set()
    }

  /**
    * extranct the left and the right parts from the given formula
    * @param f the formula to be extracted
    * @return as set contains the left and the right parts of f if f is and And formula. an empty set otherwise
    */
  def extractFromAnd(f: Formula): FormulaSet = f match {
    case And(l, r) => Set(l, r)
    case _         => Set()
  }

  /**
    * construct the th of a given set of formulas.
    * the th of a set of formulas is the set of all formulas that can be infered from this set using the axioms
    * and the rules of zero order logic
    * @param e a set of formulas
    * @return the theory of the give set
    * @note it's a proof that I'm an idiot,
    *       just don't look at it if you have some logic basics. otherwise you'll be able to unstar the project
    */
  def th(e: FormulaSet): FormulaSet = {
    val th = mutable.Set[Formula]()
    th ++= e
    val (literals, o1) = e.partition(_.isInstanceOf[Literal])
    val (formulas, negations) = o1.partition(_.isInstanceOf[BinaryFormula])
    val (ands, o3) =
      formulas.partition(_.asInstanceOf[BinaryFormula].op == LogicAnd)
    val (ors, implies) =
      o3.partition(_.asInstanceOf[BinaryFormula].op == LogicOr)
    th ++= ands.flatMap(extractFromAnd)
    while (th.size != (th ++= implies.flatMap(modusPonun(_, th))).size) {}
    th
  }
}

object True extends Formula
object False extends Formula

/**
  * a Binary Formula is a well formed formulas defined as folows :
  * If f1 and f2 are well-formed formulas, then so are f1 ∨ f2, f1 ∧ f2, f1 ⊃ f2, and f1 <=> f2.
  */
class BinaryFormula(val l: Formula, val r: Formula, val op: LogicFunction)
    extends Formula {
  def unapply(arg: BinaryFormula): Option[(Formula, Formula, LogicFunction)] =
    Option((l, r, op))
  def apply(l: Formula, r: Formula, op: LogicFunction) =
    new BinaryFormula(l, r, op)
}

/**
  * a Literal is an atomic proposition
  * @example P, Q, R .. etc
  * @param symbol the symbol that contians the name of the formulas
  * @note it is recommended to use predefined literals in the [[com.usthb.logic.Literals]] object
  */
case class Literal(symbol: Symbol) extends Formula {
  def num: Int = symbol.name.map(_.toInt - 'A'.toInt + 1).sum
}

/**
  * a Negation is a well formed fromulas defined as follows :
  * If f is a well-formed formula, then so is !f.
  * @param f the formula to be negated
  */
case class Negation private (f: Formula) extends Formula
object Negation {
  def apply(f: Formula): Formula = {
    f match {
      case Negation(notF) => notF
      case _              => new Negation(f)
    }
  }
}

/**
  * A Formulas composed for l and r
  * @param l the left part of the operation
  * @param r the right part of the operation
  */
case class And(override val l: Formula, override val r: Formula)
    extends BinaryFormula(l, r, LogicAnd)

/**
  * A Formulas composed for l or r
  * @param l the left part of the operation
  * @param r the right part of the operation
  */
case class Or(override val l: Formula, override val r: Formula)
    extends BinaryFormula(l, r, LogicOr)

/**
  * A Formulas composed for l implies r
  * @param l the left part of the operation
  * @param r the right part of the operation
  */
case class Implies(override val l: Formula, override val r: Formula)
    extends BinaryFormula(l, r, LogicImplies)

/**
  * A Formulas composed for l equivalent r
  * @param l the left part of the operation
  * @param r the right part of the operation
  */
case class Equivalent(override val l: Formula, override val r: Formula)
    extends BinaryFormula(l, r, LogicEquivalent)

/**
  * binary functions to construct new formulas that have a name (for converting to string properly)
  * @param name a name of the operation (using when converting a [[com.usthb.logic.Formula]] to string)
  * @param op a binray operation to construct new fromuals
  * @example f1 and f2,  f1 or f2 ...etc
  */
case class LogicFunction(name: String, op: (Formula, Formula) => Formula)

/**
  * implementation of logical and operator for [[com.usthb.logic.LogicFunction]]
  */
object LogicAnd extends LogicFunction("∧", _ and _)

/**
  * implementation of logical or operator for [[com.usthb.logic.LogicFunction]]
  */
object LogicOr extends LogicFunction("∨", _ or _)

/**
  * implementation of logical implies operator for [[com.usthb.logic.LogicFunction]]
  */
object LogicImplies extends LogicFunction("⊃", _ implies _)

/**
  * implementation of logical equivalent operator for [[com.usthb.logic.LogicFunction]]
  */
object LogicEquivalent extends LogicFunction("≡", _ equivalent _)
