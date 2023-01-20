package pl.wojciechkarpiel.tableaux
package unification

import lang.Term.NamedVar
import lang.Term
import unification.Unifier.*
import unification.Unifier.UnificationResult.*
import unification.Unifier.UnifierTerm.*

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.Stack

class UnifierTest extends AnyFlatSpec with should.Matchers {

  private val treeName = NamedVar("Tree")
  private val x: Unifiable = Unifiable(new Term.Unifiable())
  private val y: Unifiable = Unifiable(new Term.Unifiable())
  private val D: Tree = Tree(NamedVar("D"), Seq(), true)
  private val E: Tree = Tree(NamedVar("E"), Seq(), true)

  "Unifier" should "unify" in {
    val result = unify(Tree(treeName, Seq(y, D), true), Tree(treeName, Seq(x, x), true))
    result match
      case UnificationFailure => fail()
      case UnificationSuccess(substitution) =>
        substitution.size should be(2)
        assert(substitution.find(x).contains(D))
        assert(substitution.find(y).contains(D))
  }
  it should "actually work" in {
    val result = unify(Tree(treeName, Seq(x, y), true), Tree(treeName, Seq(D, E), true))
    result match
      case UnificationFailure => fail()
      case UnificationSuccess(substitution) =>
        substitution.size should be(2)
        assert(substitution.find(x).contains(D))
        assert(substitution.find(y).contains(E))
  }

}
