package pl.wojciechkarpiel.tableaux
package example

import pl.wojciechkarpiel.tableaux.lang.Term
import pl.wojciechkarpiel.tableaux.lang.Formula
import pl.wojciechkarpiel.tableaux.lang.Formula.*
import pl.wojciechkarpiel.tableaux.lang.Term.*
import pl.wojciechkarpiel.tableaux.tree.Tree

object Example {
  def example(): Unit = {
    val isDrinkerParadoxValid: Boolean = Tree.isTautology("∃x.(∀y.((Pije(x) ⇒ Pije(y))))", searchBound = 3)
    println(s"Drinker paradox is ${if isDrinkerParadoxValid then "valid" else "invalid"}")

    val P = PredicateName("P")
    val x = NamedVar("x")
    val excludedMiddle = ForAll(x, Or(P(x), Not(P(x))))
    println(s"Hand-crafted formula: $excludedMiddle")
    val excludedMiddleHolds: Boolean = Tree.isTautology(excludedMiddle, searchBound = 3)
    println(s"Excluded middle ${if excludedMiddleHolds then "holds" else "does not hold"}")
  }
}
