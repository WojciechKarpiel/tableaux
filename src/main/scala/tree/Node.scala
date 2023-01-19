package pl.wojciechkarpiel.tableaux
package tree

import lang.Formula
import tree.RuleType.Gamma

final class Node(var formula: Formula) {

  val originalFormula: Formula = formula

  private var hasExpanded: Boolean = false

  def canExpand: Boolean = !hasExpanded || RuleType(formula) == Gamma

  def restoreOriginalFormula(): Unit = formula = originalFormula

  def expand(): Unit = if canExpand then {
    hasExpanded = true
    val expansion = Expansion(formula)
  }

}
