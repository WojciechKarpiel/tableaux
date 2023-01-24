package pl.wojciechkarpiel.tableaux
package unification

import lang.Formula.*
import lang.Term.{Function, FunctionName, NamedVar, Variable}
import lang.{Formula, Term}
import unification.Unifier
import unification.Unifier.UnificationResult.{UnificationFailure, UnificationSuccess}
import unification.Unifier.UnifierTerm.{Tree, Unifiable}
import unification.Unifier.{Substitution, UnifierTerm}
import util.MetadataHolder

object FormulaInterop {

  def apply(predicate: Predicate): UnifierTerm = Tree(predicate.name.name, predicate.args.map(apply), MetadataHolder(true))

  def apply(term: Term): UnifierTerm = term match
    case unifiable: Term.Unifiable => Unifiable(unifiable)
    case variable: NamedVar => Tree(variable, Seq(), MetadataHolder(false))
    case variable: Term.InternVar => Tree(variable, Seq(), MetadataHolder(false))
    case Term.Function(name, args) => Tree(name.name, args.map(apply), MetadataHolder(true))

  def toLogicTerm(t: UnifierTerm): Term = t match
    case UnifierTerm.Tree(name, branches, meta) =>
      if meta.isFunction then Function(FunctionName(name), branches.map(toLogicTerm)) else name
    case UnifierTerm.Unifiable(id) => id


  private def substitute(formula: Formula, substitution: Substitution) = {
    def substituteTerm(t: Term): Term = {
      t match
        case Term.NamedVar(name) => Term.NamedVar(name)
        case u: Term.Unifiable => substitution.find(Unifiable(u)).map(toLogicTerm).getOrElse(u)
        case i: Term.InternVar => i
        case Term.Function(name, args) => Term.Function(name, args.map(substituteTerm))
    }

    def substituteFormula(f: Formula): Formula = {
      f match
        case Predicate(name, args) => Predicate(name, args.map(substituteTerm))
        case Not(formula) => Not(substituteFormula(formula))
        case ForAll(variable, body) => ForAll(variable, substituteFormula(body))
        case Exists(variable, body) => Exists(variable, substituteFormula(body))
        case And(a, b) => And(substituteFormula(a), substituteFormula(b))
        case Or(a, b) => Or(substituteFormula(a), substituteFormula(b))
        case Equivalent(a, b) => Equivalent(substituteFormula(a), substituteFormula(b))
        case Implies(premise, conclusion) => Implies(substituteFormula(premise), substituteFormula(conclusion))
    }

    substituteFormula(formula)
  }

}
