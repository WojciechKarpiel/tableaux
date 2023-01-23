package pl.wojciechkarpiel.tableaux
package tree

import lang.{Formula, NormalizedHeadFormula}

enum RuleType {
  // ordering implied node expanion ordering
  case AlphaLike
  case Alpha
  case Beta
  case Delta
  case Gamma
}

object RuleType {
  private def ofNormalizedFormula(formula: NormalizedHeadFormula): RuleType = formula match
    case Formula.Predicate(_, _) => AlphaLike
    case Formula.Not(_) => AlphaLike
    case Formula.ForAll(_, _) => Gamma
    case Formula.Exists(_, _) => Delta
    case Formula.And(_, _) => Alpha
    case Formula.Or(_, _) => Beta

  def apply(formula: Formula): RuleType = {
    val normalized = Normalization.normalizeHead(formula)
    if normalized == formula then ofNormalizedFormula(normalized) else AlphaLike
  }
}