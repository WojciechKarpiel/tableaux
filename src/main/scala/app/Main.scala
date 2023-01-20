package pl.wojciechkarpiel.tableaux
package app

import tree.Tree

object Main {

  def main(args: Array[String]): Unit = {
    println("elo")
    // todo parsing neg lol
    //    val t: Tree = new Tree("A or B and (~C) -> exists x. P(x)")
    //    t.expandNonGamma()
    //    t.expandGammaOnce()
    //    t.expandNonGamma()
    //    t.printTree()

    //    val nats = "N(O) ∧ ∀i.((N(i) ⇒ N(s(i)))) ⇒ N(s(s(s(O))))" // todo parser
    val nats = "(N(O) ∧ (∀i.((N(i) ⇒ N(s(i))))) ⇒ N(s(s(s(O)))))"
    val barowy = "exists x. forall y. (Pije(x) => Pije(y))"

    val tb = new Tree(barowy)
    println(tb.solve(2))
    tb.printTree()

    val tn = new Tree(nats)
    println(tn.solve(3))
    tn.printTree()
  }
}
