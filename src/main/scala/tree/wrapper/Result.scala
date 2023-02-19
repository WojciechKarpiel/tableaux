package pl.wojciechkarpiel.tableaux
package tree.wrapper

import lang.Formula.Predicate
import tree.Tree
import tree.wrapper.PropositionalHandler.PropositionalResult

enum Result: // TODO let Tree be some wrapper
  case Tautology(proof: Tree)
  case NegatedTautology(proof: Tree)
  case Neither(trueValuation: Map[Predicate, Boolean], falseValuation: Map[Predicate, Boolean])
  case DontKnow(searchBound: Int, tautologyTree: Tree, negatedTatologyTree: Tree) // TODO try to find counter-model instead of dunno

object Result:
  def apply(propositional: PropositionalHandler.PropositionalResult): Result = propositional match
    case PropositionalResult.Tautology(proof) => Tautology(proof)
    case PropositionalResult.NegatedTautology(proof) => NegatedTautology(proof)
    case PropositionalResult.Neither(trueValuation, falseValuation) => Neither(trueValuation, falseValuation)