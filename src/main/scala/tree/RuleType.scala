package pl.wojciechkarpiel.tableaux
package tree

import lang.{Formula, NormalizedHeadFormula}

enum RuleType:
  // ordering implies node expansion ordering
  case AlphaLike
  case Alpha
  case Beta
  case Delta
  case Gamma
  case Pi
  case Upsilon

object RuleType {
  private def ofNormalizedFormula(formula: NormalizedHeadFormula): RuleType = formula match
    case Formula.Predicate(_, _) => AlphaLike
    case Formula.Not(_) => AlphaLike
    case Formula.ForAll(_, _) => Gamma
    case Formula.Exists(_, _) => Delta
    case Formula.And(_, _) => Alpha
    case Formula.Or(_, _) => Beta
    case Formula.Possibly(_) => Pi
    case Formula.Necessarily(_) => Upsilon

  def apply(formula: Formula): RuleType = {
    val normalized = Normalization.normalizeHead(formula)
    if normalized == formula then ofNormalizedFormula(normalized) else AlphaLike
  }
}