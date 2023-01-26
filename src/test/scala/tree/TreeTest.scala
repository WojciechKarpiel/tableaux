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
    assert(tb.solve(2).isDefined)
    val tn = new Tree(threeExists)
    assert(tn.solve(3).isDefined)
  }

  it should "fail when search depth is not enough to solve the problem" in {
    val tn = new Tree(threeExists)
    assert(tn.solve(2).isEmpty)
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
        tree.solve(3).isDefined
      })
    )
  }


  it should "handle some random cases" in {
    val pcz = "(forall x. Id(x, x))"
    val dcza = "(forall x. exists y. Id(x,y))"
    val dczaNie = "(exists x. forall y. Id(x,y))"

    val ok = s"$pcz -> $dcza"
    val nOk = s"( $pcz -> $dczaNie)"

    val tOk = new Tree(ok)
    val rNok = new Tree(nOk)
    val rNok2 = new Tree("~(" + nOk + ")")

    assert(tOk.solve(1).isDefined)
    assert(rNok.solve(2).isEmpty)
    assert(rNok2.solve(2).isEmpty)
  }

  def testSolvable(input: String, searchBound: Int, expected: Boolean = true): Unit =
    Tree.isTautology(input, searchBound) should be(expected)

  it should "random cases 2" in {
    val ok = "∀x.(∃y.((P(x) ⇔ P(y))))"
    val unprovable = "∃x.(∀y.((P(x) ⇔ P(y))))"

    testSolvable(ok, 1)
    testSolvable(unprovable, 2, false)
    testSolvable(s"~($unprovable)", 2, false)

  }

  it should "delay unification until a solution is found" in {
    val ss =
      Seq(
        "~(forall x. (P(a) and P(x) and (~P(a) or ~P(b))))",
        "~(forall x. (P(a) and P(x) and (~P(a) or ~P(b))))",
      )
    assert(ss.forall(s => new Tree(s).solve(1).isDefined))
  }
}
