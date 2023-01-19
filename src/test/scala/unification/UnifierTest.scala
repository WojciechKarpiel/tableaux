package pl.wojciechkarpiel.tableaux
package unification

import lang.Term.NamedVar
import unification.Unifier.*
import unification.Unifier.UnificationResult.*
import unification.Unifier.UnifierTerm.*

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.Stack

class UnifierTest extends AnyFlatSpec with should.Matchers {

  private val treeName = NamedVar("Tree")
  private val x: Unifiable = Unifiable(NamedVar("x"))
  private val y: Unifiable = Unifiable(NamedVar("y"))
  private val D: Tree = Tree(NamedVar("D"), Seq())

  "Unifier" should "unify" in {
    val result = unify(Tree(treeName, Seq(y, D)), Tree(treeName, Seq(x, x)))
    result match
      case UnificationFailure => fail()
      case UnificationSuccess(substitution) =>
        substitution.size should be(2)
        assert(substitution.find(x).contains(D))
        assert(substitution.find(y).contains(D))
  }

}
