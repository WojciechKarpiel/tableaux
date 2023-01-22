package pl.wojciechkarpiel.tableaux
package api

import lang.Formula
import tree.Tree

object Solver {

  def isTautology(formula: String, searchBound: Int): Boolean = Tree.isTautology(formula, searchBound)

  def isTautology(formula: Formula, searchBound: Int): Boolean = Tree.isTautology(formula, searchBound)
}
