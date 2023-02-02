package pl.wojciechkarpiel.tableaux
package util

import lang.Formula.*
import lang.Term.*
import lang.{Formula, NormalizedHeadFormula, Term}
import tree.Normalization

object FormulaUtil {
  def replaceVariable(variable: Variable, replacement: Term, formula: Formula): Formula = {
    def replace(formula: Formula): Formula = formula match {
      case Predicate(name, args) => Predicate(name, args.map(arg => replaceVariableInTerm(variable, replacement, arg)))
      case Not(formula) => Not(replace(formula))
      case f@ForAll(v, body) => if v == variable then f else ForAll(v, replace(body))
      case e@Exists(v, body) => if v == variable then e else Exists(v, replace(body))
      case And(a, b) => And(replace(a), replace(b))
      case Or(a, b) => Or(replace(a), replace(b))
      case Equivalent(a, b) => Equivalent(replace(a), replace(b))
      case Implies(premise, conclusion) => Implies(replace(premise), replace(conclusion))
      case Necessarily(formula) => Necessarily(replace(formula))
      case Possibly(formula) => Possibly(replace(formula))
    }

    replace(formula)
  }

  def replaceVariableInTerm(variable: Variable, replacement: Term, term: Term): Term = term match
    case v: Variable => if v == variable then replacement else v
    case Function(name, args) => Function(name, args.map(arg => replaceVariableInTerm(variable, replacement, arg)))

  def freeVariables(formula: Formula, scopedVariables: Set[Variable]): Set[Variable] =
    Normalization.normalizeHead(formula) match
      case Predicate(_, args) =>
        val value = args.map(termFreeVariables)
        val r = value.fold(Set())(_ ++ _).diff(scopedVariables)
        r
      case Not(formula) => freeVariables(formula, scopedVariables)
      case ForAll(variable, body) => freeVariables(body, scopedVariables + variable)
      case Exists(variable, body) => freeVariables(body, scopedVariables + variable)
      case Possibly(formula) => freeVariables(formula, scopedVariables)
      case Necessarily(formula) => freeVariables(formula, scopedVariables)
      case And(a, b) => freeVariables(a, scopedVariables) ++ freeVariables(b, scopedVariables)
      case Or(a, b) => freeVariables(a, scopedVariables) ++ freeVariables(b, scopedVariables)

  def termFreeVariables(term: Term): Set[Variable] = term match
    case variable: Variable => Set(variable)
    case Function(_, args) => args.map(termFreeVariables).fold(Set())(_ ++ _)
}
