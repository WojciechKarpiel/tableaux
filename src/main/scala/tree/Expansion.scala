package pl.wojciechkarpiel.tableaux
package tree

import lang.Formula.*
import lang.Term.*
import lang.{Formula, Term}
import util.FormulaUtil

case class Branch(formulas: Seq[Formula]) extends AnyVal

object Branch {
  def apply(singleFormula: Formula): Branch = Branch(Seq(singleFormula))
}

case class Expansion(branches: Seq[Branch]) extends AnyVal

object Expansion {
  private def empty: Expansion = Expansion(Seq())

  private def singleBranch(singleBranch: Branch): Expansion = Expansion(Seq(singleBranch))

  def apply(formula: Formula): Expansion =
    val normalized = formula.normalizeHead
    if normalized != formula then Expansion.singleBranch(Branch(normalized))
    else normalized match
      case Formula.Predicate(_, _) => Expansion.empty
      case Formula.Not(_) => Expansion.empty
      case forall: Formula.ForAll =>
        Expansion.singleBranch(Branch(gammaExpansion(forall)._2))
      case Formula.Exists(variable, body) =>
        val freeVariablesSet = FormulaUtil.freeVariables(body, Set(variable))
        val freeVariables: Seq[Term] = freeVariablesSet.toSeq
        val skolemConstantId = FunctionName(new InternVar())
        val newVariable = Function(skolemConstantId, freeVariables)
        val formula1 = FormulaUtil.replaceVariable(variable, newVariable, body)
        Expansion.singleBranch(Branch(formula1))
      case Formula.And(a, b) => Expansion.singleBranch(Branch(Seq(a, b)))
      case Formula.Or(a, b) => Expansion(Seq(Branch(a), Branch(b)))

  /**
   * @return new unifiable term and expanded formula containing the term
   */
  def gammaExpansion(forAll: ForAll): (Unifiable, Formula) =
    val ForAll(variable, body) = forAll
    val unifiable = new Unifiable()
    unifiable -> FormulaUtil.replaceVariable(variable, unifiable, body)
}