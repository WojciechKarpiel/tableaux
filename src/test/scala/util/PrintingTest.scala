package pl.wojciechkarpiel.tableaux
package util


import lang.Formula.*
import lang.Term.*
import parser.Parser

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.Stack


class PrintingTest extends AnyFlatSpec with should.Matchers {

  "Printer" should "print the formulas" in {
    val threeExists = Parser.parseOrThrow("(N(0) ∧ (∀i.((N(i) ⇒ N(s(i))))) ⇒ N(s(s(s(0)))))")
    val barowy = Parser.parseOrThrow("exists x. forall y. (Pije(x) => Pije(y))")

    assert(Printing.toString(barowy) == "∃x.(∀y.((Pije(x) ⇒ Pije(y))))")
    assert(Printing.toString(threeExists) == "((N(0) ∧ ∀i.((N(i) ⇒ N(s(i))))) ⇒ N(s(s(s(0)))))")
  }
}
