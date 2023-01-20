package pl.wojciechkarpiel.tableaux
package tree

import lang.Formula.*
import lang.Term.*

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.Stack


class TreeTest extends AnyFlatSpec with should.Matchers {

  val threeExists = "(N(O) ∧ (∀i.((N(i) ⇒ N(s(i))))) ⇒ N(s(s(s(O)))))"

  "Tree" should "handle classic examples" in {
    val barowy = "exists x. forall y. (Pije(x) => Pije(y))"

    val tb = new Tree(barowy)
    assert(tb.solve(2))
    val tn = new Tree(threeExists)
    assert(tn.solve(3))
  }

  it should "fail when search depth is not enough to solve the problem" in {
    val tn = new Tree(threeExists)
    assert(!tn.solve(2))
  }

  it should "not get stuck by picking wrong terms to unify (making other branches impossible)" in {
    val variants = Seq(
      "!((forall x . P(x)) and P(a) and ((~P(b)) or (~P(a))))",
      "!((forall x . P(x)) and P(a) and ((~P(a)) or (~P(b))))",
      "!(P(a) and (forall x . P(x)) and ((~P(a)) or (~P(b))))",
      "!(P(a) and (forall x . P(x)) and ((~P(b)) or (~P(a))))"
    )

    assert(
      variants.forall(input => {
        val tree = new Tree(input)
        tree.solve(3)
      })
    )
  }


}
