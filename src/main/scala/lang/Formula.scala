package pl.wojciechkarpiel.tableaux
package lang

import lang.Term.{NamedVar, Variable}
import util.Printing

sealed trait Formula {
  override def toString: String = Printing.toString(this)
}

sealed trait NormalizedHeadFormula extends Formula

object Formula {
  case class Predicate(name: PredicateName, args: Seq[Term]) extends NormalizedHeadFormula

  case class Not(formula: Formula) extends NormalizedHeadFormula

  case class ForAll(variable: Variable, body: Formula) extends NormalizedHeadFormula

  case class Exists(variable: Variable, body: Formula) extends NormalizedHeadFormula

  case class And(a: Formula, b: Formula) extends NormalizedHeadFormula

  case class Or(a: Formula, b: Formula) extends NormalizedHeadFormula

  case class Necessarily(formula: Formula) extends NormalizedHeadFormula

  case class Possibly(formula: Formula) extends NormalizedHeadFormula

  case class Equivalent(a: Formula, b: Formula) extends Formula

  case class Implies(premise: Formula, conclusion: Formula) extends Formula


  // helper stuff, end of actual AST defintion
  object Predicate {
    def apply(name: PredicateName, arg: Term): Predicate = Predicate(name, Seq(arg))

    def constant(name: PredicateName): Predicate = Predicate(name, Seq())
  }

  case class PredicateName(name: NamedVar) extends AnyVal {
    override def toString: String = name.toString

    def apply(args: Term*): Predicate = Predicate(this, args)
  }

  object PredicateName {
    def apply(name: String): PredicateName = PredicateName(NamedVar(name))
  }
}
