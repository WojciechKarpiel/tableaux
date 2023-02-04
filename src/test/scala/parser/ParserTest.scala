package pl.wojciechkarpiel.tableaux
package parser

import lang.Formula.*
import lang.Formula
import lang.Term.*
import lang.Term

import org.parboiled2.{ParseError, Parser as ParboiledParser}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.collection.mutable.Stack
import scala.util.{Failure, Success}


class ParserTest extends AnyFlatSpec with should.Matchers {

  private val x = NamedVar("x")
  private val f = FunctionName(NamedVar("f"))
  private val d: Function = Function(FunctionName(NamedVar("d")), Seq())
  private val b = Function(FunctionName(NamedVar("b")), Seq())
  private val c = Function(FunctionName(NamedVar("c")), Seq())
  private val P = PredicateName("P")
  private val E = PredicateName("E")
  private val A = Predicate(PredicateName("A"), Seq())
  private val B = Predicate(PredicateName("B"), Seq())
  private val C = Predicate(PredicateName("C"), Seq())
  private val D = Predicate(PredicateName("D"), Seq())


  "A Parser" should "parse stuff" in {
    Parser.run("forall x. P(x) && ~E(f(c))").get should be(ForAll(x, And(Predicate(P, Seq(x)), Not(Predicate(E, Seq(Function(f, Seq(c))))))))
    Parser.run("A and B  and (A -> B) -> C").get should be(Implies(And(A, And(B, Implies(A, B))), C))
    Parser.run("A -> B -> C").get should be(Implies(A, Implies(B, C)))
    Parser.run("forall x P(x) and P(b) and C -> D and A").get should be
    Implies(ForAll(x, And(Predicate(P, Seq(x)), And(Predicate(P, Seq(b)), C))), And(D, A))


    test("~(forall x. (P(d) and P(x) and (~P(d) or ~P(b))))",
      Not(ForAll(x, And(Predicate(P, d), And(Predicate(P, x), Or(Not(Predicate(P, d)), Not(Predicate(P, b))))))))
  }


  it should "failing cases" in {
    test("~A and B", And(Not(A), B))
    test("N(O) ∧ ∀i.((N(i) ⇒ N(s(i)))) ⇒ N(s(s(s(O))))", Parser.run("(N(O) ∧ (∀i.((N(i) ⇒ N(s(i))))) ⇒ N(s(s(s(O)))))").get)
  }

  it should "parse modal logic expressions" in {
    test("<>A ", Possibly(A))
    test("◇□A ", Possibly(Necessarily(A)))
    test("[]A -> A", Implies(Necessarily(A), A))
    test("forall x. []P(x) -> [] forall x.P(x) and A", Implies(ForAll(x, Necessarily(P(x))), Necessarily(ForAll(x, And(P(x), A)))))
    test("forall x. []P(x) -> [] (forall x.P(x)) and A", Implies(ForAll(x, Necessarily(P(x))), Necessarily(And(ForAll(x, P(x)), A))))
  }

  private def test(input: String, expected: Formula): Unit = {
    val p = Parser(input)
    p.runParser() match
      case Failure(e: ParseError) => fail(p.formatError(e))
      case Success(value) => value should be(expected)
      case Failure(other) => throw other
  }

}