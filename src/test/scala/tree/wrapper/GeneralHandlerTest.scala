package pl.wojciechkarpiel.tableaux
package tree.wrapper

import lang.Formula.*
import lang.Term.*
import tree.wrapper.GeneralHandler.handle
import tree.wrapper.Result.*

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class GeneralHandlerTest extends AnyFlatSpec with should.Matchers {

  "General Handler" should "work" in {
    handle("a", 3) match
      case Neither(_, _) => //OK
      case _ => fail()

    handle("[]p => []p", 0) match
      case Tautology(_) => // OK
      case _ => fail()

    handle("a and !a", 3) match
      case NegatedTautology(_) => //OK
      case _ => fail()

    handle("forall x P(x)", 2) match
      case DontKnow(_, _, _) => // OK
      case _ => fail()

    handle("exists x (P(x) and !P(x))", searchBound = 1) match
      case NegatedTautology(_) => // OK
      case _ => fail()

    handle("exists x (P(x) or !P(x))", searchBound = 1) match
      case Tautology(_) => // OK
      case _ => fail()

    handle("(exists x P(x)) and (forall x !P(x))", 2) match
      case NegatedTautology(_) => // OK
      case _ => fail()
  }
}
