package pl.wojciechkarpiel.tableaux
package lang

import lang.Term.{NamedVar, Variable}
import util.Printing

sealed trait Formula

sealed trait NormalizedHeadFormula extends Formula

object Formula {
  /**
   * Equality. If the equality symbol is considered part of logic, and t1 and t2 are terms, then t1 = t2 is a formula.
   */
  case class Equal(a: Term, b: Term) { // TODO implement equality handling
    override def toString: String = s"($a = $b)"
  }

  /**
   * Predicate symbols. If P is an n-ary predicate symbol and t1, ..., tn are terms then P(t1,...,tn) is a formula.
   */
  case class Predicate(name: PredicateName, args: Seq[Term]) extends NormalizedHeadFormula {

    def arity: Int = args.size

    def isAtomic: Boolean = arity == 0

    override def toString: String = Printing.printFunctionLike(name.name, args)

    def jName: String = name.name.name
  }

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

  /**
   * Negation. If φ \varphi is a formula, then ¬ φ {\displaystyle \lnot \varphi } is a formula.
   */
  case class Not(formula: Formula) extends NormalizedHeadFormula {

    override def toString: String = "(¬" + formula + ")"
  }

  case class ForAll(variable: Variable, body: Formula) extends NormalizedHeadFormula {
    override def toString: String = "∀" + variable + ".(" + body + ")"
  }

  case class Exists(variable: Variable, body: Formula) extends NormalizedHeadFormula {
    override def toString: String = "∃" + variable + ".(" + body + ")"
  }

  case class And(a: Formula, b: Formula) extends NormalizedHeadFormula {
    override def toString: String = "(" + a + " ∧ " + b + ")"
  }

  case class Or(a: Formula, b: Formula) extends NormalizedHeadFormula {
    override def toString: String = "(" + a + " ∨ " + b + ")"
  }

  case class Equivalent(a: Formula, b: Formula) extends Formula {

    override def toString: String = "(" + a + " ⇔ " + b + ")"
  }

  case class Implies(premise: Formula, conclusion: Formula) extends Formula {

    override def toString: String = s"($premise ⇒ $conclusion)"
  }
}

/*
Binary connectives. If φ \varphi and ψ \psi are formulas, then ( φ → ψ \varphi \rightarrow \psi ) is a formula. Similar rules apply to other binary logical connectives.
Quantifiers. If φ \varphi is a formula and x is a variable, then ∀ x φ \forall x\varphi (for all x, φ \varphi holds) and ∃ x φ \exists x\varphi (there exists x such that φ \varphi ) are formulas.
*/