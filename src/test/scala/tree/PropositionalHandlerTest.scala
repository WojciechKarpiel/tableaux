package pl.wojciechkarpiel.tableaux
package tree

import lang.Formula.*
import lang.Term.*
import parser.Parser.parseOrThrow
import tree.PropositionalHandler.PropositionalResult.*
import tree.PropositionalHandler.{PropositionalResult, StrongerLogic}

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*


class PropositionalHandlerTest extends AnyFlatSpec with should.Matchers {

  "Propositional handler" should "abort in case of more expressive logic" in {
    PropositionalHandler.handle("forall x. P(x)") should be(Left(StrongerLogic))
    PropositionalHandler.handle("exists x. P(x)") should be(Left(StrongerLogic))
    PropositionalHandler.handle("[] P(x)") should be(Left(StrongerLogic))
    PropositionalHandler.handle("<> P(x)") should be(Left(StrongerLogic))
    PropositionalHandler.handle("[] forall x. P(x)") should be(Left(StrongerLogic))
    PropositionalHandler.handle("forall x. [] P(x)") should be(Left(StrongerLogic))
  }

  it should "recognize a tautology" in {
    assertTautology("a or !a")
  }

  it should "recognize negated tautology" in {
    assertNegatedTautology("a and !a")
  }

  it should "find counterexamples" in {
    assertCounterexamples("x", Map("x" -> true), Map("x" -> false))
    assertCounterexamples("!x", Map("x" -> false), Map("x" -> true))
    assertCounterexamples(
      "a and ((!a or b) and !c)",
      Map("a" -> true, "b" -> true, "c" -> false),
      Map("a" -> false)
    )
  }

  def assertCounterexamples(
                             input: String,
                             trueValuation: Map[String, Boolean],
                             falseValuation: Map[String, Boolean]
                           ): Unit =
    PropositionalHandler.handle(input) match
      case Right(Neither(trueV, falseV)) =>
        assert(trueV == trueValuation.map { case (k, v) => parseOrThrow(k) -> v })
        assert(falseV == falseValuation.map { case (k, v) => parseOrThrow(k) -> v })
      case _ => fail()

  def assertNegatedTautology(input: String): Unit =
    PropositionalHandler.handle(input) match
      case Right(NegatedTautology(_)) => // OK
      case _ => fail()

  def assertTautology(input: String): Unit =
    PropositionalHandler.handle(input) match
      case Right(Tautology(_)) => // OK
      case _ => fail()
}
