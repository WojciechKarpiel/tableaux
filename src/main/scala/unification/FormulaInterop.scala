package pl.wojciechkarpiel.tableaux
package unification

import lang.Term
import lang.Term.{Function, FunctionName, NamedVar, Variable}
import unification.Unifier.UnificationResult.{UnificationFailure, UnificationSuccess}
import unification.Unifier.{Substitution, UnifierTerm}
import unification.Unifier
import unification.Unifier.UnifierTerm.Unifiable
import unification.Unifier.UnifierTerm.Tree

import pl.wojciechkarpiel.tableaux.lang.Formula.Predicate
import pl.wojciechkarpiel.tableaux.lang.Formula
import pl.wojciechkarpiel.tableaux.lang.Term

object FormulaInterop {

  def apply(predicate: Predicate): UnifierTerm = Tree(NamedVar(predicate.name.name), predicate.args.map(apply), true)

  def apply(term: Term): UnifierTerm = term match
    case unifiable: Term.Unifiable => Unifiable(unifiable)
    case variable: NamedVar => Tree(variable, Seq(), false)
    case variable: Term.InternVar => Tree(variable, Seq(), false)
    case Term.Function(name, args) => Tree(name.name, args.map(apply), true)

  def fixSub(substitution: Substitution): Map[Term.Unifiable, Term] = {
    substitution.raw.map { case (a, b) => a.id -> toLogicTerm(b) }.toMap
  }

  def toLogicTerm(t: UnifierTerm): Term = t match
    case UnifierTerm.Tree(name, branches, meta) =>
      if meta then Function(FunctionName(name), branches.map(toLogicTerm)) else name
    case UnifierTerm.Unifiable(id) => id

}
