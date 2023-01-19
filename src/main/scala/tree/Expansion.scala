package pl.wojciechkarpiel.tableaux
package tree

import lang.Formula.*
import lang.{Formula, Term}
import lang.Term.*
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
      case Formula.ForAll(variable, body) =>
        val unifiable = Function(FunctionName(new Unifiable()), Seq())
        Expansion.singleBranch(Branch(FormulaUtil.replaceVariable(variable, unifiable, body)))
      case Formula.Exists(variable, body) =>
        val freeVariablesSet = FormulaUtil.freeVariables(body, Set(variable))
        val freeVariables: Seq[Term] = freeVariablesSet.toSeq
        val skolemConstantId = FunctionName(new InternVar())
        val newVariable = Function(skolemConstantId, freeVariables)
        Expansion.singleBranch(Branch(FormulaUtil.replaceVariable(variable, newVariable, body)))
      case Formula.And(a, b) => Expansion.singleBranch(Branch(Seq(a, b)))
      case Formula.Or(a, b) => Expansion(Seq(Branch(a), Branch(b)))
}