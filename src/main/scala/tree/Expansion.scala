package pl.wojciechkarpiel.tableaux
package tree

import lang.Formula.*
import lang.Term.*
import lang.{Formula, Term}
import tree.Expansion.InterplanetaryExpansion.{AllReachableWorlds, IntoNewWorld, SameWorld}
import util.FormulaUtil

case class Branch(formulas: Seq[Formula]) extends AnyVal

object Branch {
  def apply(singleFormula: Formula): Branch = Branch(Seq(singleFormula))
}

class Expansion(val branches: Seq[Branch]) extends AnyVal

object Expansion {

  // TODO drut przerobić lepiej
  def apply(branches: Seq[Branch]): InterplanetaryExpansion = SameWorld(new Expansion(branches))

  private def empty: InterplanetaryExpansion = Expansion.apply(Seq[Branch]())

  private def singleBranch(singleBranch: Branch): InterplanetaryExpansion = Expansion(Seq(singleBranch))

  def apply(formula: Formula): InterplanetaryExpansion =
    val normalized = Normalization.normalizeHead(formula)
    if normalized != formula then Expansion.singleBranch(Branch(normalized))
    else normalized match
      case Predicate(_, _) => Expansion.empty
      case Not(_) => Expansion.empty
      case ForAll(variable, body) =>
        Expansion.singleBranch(Branch(FormulaUtil.replaceVariable(variable, new Unifiable(), body)))
      case Exists(variable, body) =>
        val freeVariables = FormulaUtil.freeVariables(body, Set(variable))
        val skolemConstantId = FunctionName(new InternVar())
        val newTerm = Function(skolemConstantId, freeVariables.toSeq)
        Expansion.singleBranch(Branch(FormulaUtil.replaceVariable(variable, newTerm, body)))
      case And(a, b) => Expansion.singleBranch(Branch(Seq(a, b)))
      case Or(a, b) => Expansion(Seq(Branch(a), Branch(b)))
      case Possibly(formula) => IntoNewWorld(formula)
      case Necessarily(formula) => AllReachableWorlds(formula)

  /**
   * @return new unifiable term and expanded formula containing the term
   */
  def gammaExpansion(forAll: ForAll): (Unifiable, Formula) =
    val ForAll(variable, body) = forAll
    val unifiable = new Unifiable()
    unifiable -> FormulaUtil.replaceVariable(variable, unifiable, body)

  enum InterplanetaryExpansion:
    case SameWorld(expansion: Expansion)
    case AllReachableWorlds(formula: Formula)
    case IntoNewWorld(formula: Formula)
}