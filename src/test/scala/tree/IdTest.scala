package pl.wojciechkarpiel.tableaux
package tree

import lang.Formula.*
import lang.Term.*

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.Stack


class IdTest extends AnyFlatSpec with should.Matchers {

  "Tree" should "able to chain reason about relacja równoważności" in {
    val idBasics =
      """
(
  (forall x Id(x, x)) and
  (forall x forall y ( Id(x, y) -> Id(y,x) )) and
  (forall x forall y forall z ( Id(x, y) -> Id(y,z) -> Id(x, z) ))
)""".trim

    val testRefl = idBasics + " -> Id(a, a)"
    val testSym = idBasics + " -> ( Id(a, b) -> Id(b,a) )"
    val testTrans = idBasics + " -> ( Id(a,b) -> Id(b,c) -> Id(a,c) )"

    assert(Seq(testRefl, testSym, testTrans).forall { s =>
      val t: Tree = new Tree(s)
      t.solve(3)
    })


  }
}
