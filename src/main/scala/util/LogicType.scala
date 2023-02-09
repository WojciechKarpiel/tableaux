package pl.wojciechkarpiel.tableaux
package util

import lang.{Formula, NormalizedHeadFormula}

enum LogicType:
  case Propositional
  case FirstOrder
  case Modal

  def join(other: LogicType): LogicType = if this.ordinal > other.ordinal then this else other

  def meet(other: LogicType): LogicType = if this.ordinal < other.ordinal then this else other

object LogicType:
  def ofFormula(formula: Formula): LogicType = formula match
    case Formula.Predicate(_, _) => Propositional
    case Formula.Not(formula) => Propositional.join(ofFormula(formula))
    case Formula.And(a, b) => Propositional.join(ofFormula(a).join(ofFormula(b)))
    case Formula.Or(a, b) => Propositional.join(ofFormula(a).join(ofFormula(b)))
    case Formula.Equivalent(a, b) => Propositional.join(ofFormula(a).join(ofFormula(b)))
    case Formula.Implies(premise, conclusion) => Propositional.join(ofFormula(premise).join(ofFormula(conclusion)))
    case Formula.ForAll(_, body) => FirstOrder.join(ofFormula(body))
    case Formula.Exists(_, body) => FirstOrder.join(ofFormula(body))
    case Formula.Necessarily(formula) => Modal.join(ofFormula(formula))
    case Formula.Possibly(formula) => Modal.join(ofFormula(formula))
