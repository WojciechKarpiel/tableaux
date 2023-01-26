package pl.wojciechkarpiel.tableaux
package tree

import lang.Formula.*
import lang.{Formula, NormalizedHeadFormula}

object Normalization:
  def normalizeHead(formula: Formula): NormalizedHeadFormula = formula match
    case predicate: Predicate => predicate
    case Not(formula) => normalizeHead(formula) match
      case Not(formula) => normalizeHead(formula)
      case ForAll(variable, body) => normalizeHead(Exists(variable, Not(body)))
      case Exists(variable, body) => normalizeHead(ForAll(variable, Not(body)))
      case And(a, b) => normalizeHead(Or(Not(a), Not(b)))
      case Or(a, b) => normalizeHead(And(Not(a), Not(b)))
      case predicate: Predicate => Not(predicate)
    case forAll: ForAll => forAll
    case exists: Exists => exists
    case and@And(a, b) => if a == b then normalizeHead(a) else and // comparison could be better
    case or@Or(a, b) => if a == b then normalizeHead(a) else or
    case Equivalent(a, b) => normalizeHead(Or(And(a, b), And(Not(a), Not(b))))
    case Implies(premise, conclusion) => normalizeHead(Or(Not(premise), conclusion))
