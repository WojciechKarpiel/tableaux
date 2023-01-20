package pl.wojciechkarpiel.tableaux
package app

import tree.Tree

object Main {

  def main(args: Array[String]): Unit = {

    val nats = "N(O) ∧ ∀i.((N(i) ⇒ N(s(i)))) ⇒ N(s(s(s(O))))"
    val barowy = "exists x. forall y. (Pije(x) => Pije(y))"

    val tb = new Tree(barowy)
    println(tb.solve(2))
    tb.printTree()

    val tn = new Tree(nats)
    println(tn.solve(3))
    tn.printTree()
  }
}
