package pl.wojciechkarpiel.tableaux
package tree

import lang.Formula.*
import lang.Term.*

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*


class ModalTreeTest extends AnyFlatSpec with should.Matchers {

  def testSolvable(input: String, searchBound: Int, expected: Boolean = true): Unit =
    val t = new Tree(input)
    val b = t.solve(searchBound)
    t.printTree()
    b.isDefined should be(expected)

  "Modal tree" should "handle basic cases taken from https://www.umsu.de/trees" in {
    val idd = 3
    testSolvable("∀x.◇F(x)→◇∀x.F(x)", idd, expected = false)
    testSolvable("∀x.◇F(x)→□∀x F(x)", idd, expected = false)
    testSolvable("∀x.□F(x)→◇∀x.F(x)", idd, expected = false)


    testSolvable("◇(p→q)→□p→◇q", 3)

    testSolvable(" □(p→q)→□p→□q", 1)
    testSolvable("∀x.□F(x)→□∀x.F(x)", 1)
    testSolvable("□(p→q)→□p→◇q", 3, false)
    testSolvable("◇(p→q)→◇p→◇q", 3, false)


  }
}