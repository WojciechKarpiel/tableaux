package pl.wojciechkarpiel.tableaux
package unification

import lang.Formula.Predicate
import lang.{Formula, Term}
import lang.Term.{Function, FunctionName, NamedVar, Variable}
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

  def fixSub(substitution: Substitution): Map[Term.Unifiable, Term] =
    substitution.map((a, b) => a.id -> toLogicTerm(b)).toMap

  def toLogicTerm(t: UnifierTerm): Term = t match
    case UnifierTerm.Tree(name, branches, meta) =>
      if meta.isFunction then Function(FunctionName(name), branches.map(toLogicTerm)) else name
    case UnifierTerm.Unifiable(id) => id

}
