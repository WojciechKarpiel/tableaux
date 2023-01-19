package pl.wojciechkarpiel.tableaux
package lang

import lang.Term.Variable

sealed trait Formula {
  def normalizeHead: Formula = this
}

object Formula {
  /**
   * Equality. If the equality symbol is considered part of logic, and t1 and t2 are terms, then t1 = t2 is a formula.
   */
  case class Equal(a: Term, b: Term) extends Formula

  /**
   * Predicate symbols. If P is an n-ary predicate symbol and t1, ..., tn are terms then P(t1,...,tn) is a formula.
   */
  case class Predicate(name: PredicateName, args: Seq[Term]) extends Formula {

    def arity: Int = args.size

    def isAtomic: Boolean = arity == 0
  }

  case class PredicateName(name: String) extends AnyVal

  /**
   * Negation. If φ \varphi is a formula, then ¬ φ {\displaystyle \lnot \varphi } is a formula.
   */
  case class Not(formula: Formula) extends Formula {
    override def normalizeHead: Formula = formula.normalizeHead match {
      case Not(formula) => formula.normalizeHead
      case ForAll(variable, body) => Exists(variable, body).normalizeHead
      case Exists(variable, body) => ForAll(variable, body).normalizeHead
      case And(a, b) => Or(Not(a), Not(b)).normalizeHead
      case Or(a, b) => And(Not(a), Not(b)).normalizeHead
      case implies: Implies => Not(implies.normalizeHead).normalizeHead
      case equivalent: Equivalent => Not(equivalent.normalizeHead).normalizeHead
      case _: Equal => this
      case _: Predicate => this
    }
  }

  case class ForAll(variable: Variable, body: Formula) extends Formula

  case class Exists(variable: Variable, body: Formula) extends Formula

  case class And(a: Formula, b: Formula) extends Formula // TODO normalize ordering ? this will help w unification

  case class Or(a: Formula, b: Formula) extends Formula

  case class Equivalent(a: Formula, b: Formula) extends Formula {
    override def normalizeHead: Formula = Or(And(a, b), And(Not(a), Not(b))).normalizeHead
  }

  case class Implies(premise: Formula, conclusion: Formula) extends Formula {
    override def normalizeHead: Formula = Or(Not(premise), conclusion).normalizeHead
  }
}

/*
Binary connectives. If φ \varphi and ψ \psi are formulas, then ( φ → ψ \varphi \rightarrow \psi ) is a formula. Similar rules apply to other binary logical connectives.
Quantifiers. If φ \varphi is a formula and x is a variable, then ∀ x φ \forall x\varphi (for all x, φ \varphi holds) and ∃ x φ \exists x\varphi (there exists x such that φ \varphi ) are formulas.
*/