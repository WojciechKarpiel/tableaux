package pl.wojciechkarpiel.tableaux
package tree

import lang.Formula.*
import lang.{Formula, NormalizedHeadFormula}

object Normalization {

  extension (formula: Formula) {
    private def normalizeHeadq: NormalizedHeadFormula = Normalization.normalizeHead(formula)
  }

  def normalizeHead(formula: Formula): NormalizedHeadFormula = formula match
    case formula: NormalizedHeadFormula => simplifyH(formula)
    case Equivalent(a, b) => Or(And(a, b), And(Not(a), Not(b))).normalizeHeadq
    case Implies(premise, conclusion) => Or(Not(premise), conclusion).normalizeHeadq

  private def simplifyH(formula: NormalizedHeadFormula) = formula match
    case p@Predicate(name, args) => p
    case Not(formula) => formula.normalizeHeadq match {
      case Not(formula) => formula.normalizeHeadq
      case ForAll(variable, body) => Exists(variable, Not(body)).normalizeHeadq
      case Exists(variable, body) => ForAll(variable, Not(body)).normalizeHeadq
      case And(a, b) => Or(Not(a), Not(b)).normalizeHeadq
      case Or(a, b) => And(Not(a), Not(b)).normalizeHeadq
      case predicate: Predicate => Not(predicate)
    }
    case f@ForAll(variable, body) => f
    case e@Exists(variable, body) => e
    case and@And(a, b) => if a == b then a.normalizeHeadq else and
    case or@Or(a, b) => if a == b then a.normalizeHeadq else or
}
