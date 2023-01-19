package pl.wojciechkarpiel.tableaux
package parser

import lang.Formula.*
import lang.Term.*
import lang.{Formula, Term}

import org.parboiled2.*
import org.parboiled2.support.hlist.{::, HList, HNil}

import scala.annotation.compileTimeOnly

class Parserq(val input: ParserInput) extends Parser {

  import Parserq.*

  def InputLine: Rule1[Formula] = rule {
    WhiteSpace ~ Formula ~ EOI
  }

  def Formula: Rule1[Formula] =
  //    rule(nonLeftRecursiveByNature ~ RRECSTUFF.?)
    lv2rec

  def nonLeftRecursiveByNature: Rule1[Formula] = rule(parend | Pred)


  def lvl0rec: Rule1[Formula] = nonLeftRecursiveByNature

  def lv1rec: Rule1[Formula] = rule(lvl0rec ~ (rrecAnd | rrecOr).?)

  def lv2rec: Rule1[Formula] = rule(lv1rec ~ (rrecEquiv | rrecImpies).?)

  def RRECSTUFF: FtoF = rule(
    rrecAnd | rrecOr |
      rrecEquiv | rrecImpies
  )

  def parend: Rule1[Formula] = rule(ws('(') ~ Formula ~ ws(')'))

  def and: Rule1[And] = rule {
    (nonLeftRecursiveByNature ~ ("and" | "&&") ~ WhiteSpace ~ Formula) ~> ((a: Formula, b: Formula) => And(a, b))
  }

  type Elo = Formula :: HNil //::[Formula, HNil]
  type Elo2 = ::[Formula, ::[Formula, HNil]]

  def andq: Rule[HNil, Elo] = rule {
    (("and" | "&&") /* todo word boundary */ ~ WhiteSpace ~ Formula)
  }
  /* ~> ((a: Formula, b: Formula) => And(a, b) */

  def andF: Rule[Elo2, Elo] = rule(MATCH ~> ((a: Formula, b: Formula) => And(a, b)))

  def combinedAndq: Rule2[Formula, Formula] = rule(((nonLeftRecursiveByNature ~ andq)))

  def combinedAnd: Rule1[Formula] = rule(combinedAndq ~ andF)

  type FtoF = Rule[Elo, Elo]
  //  def rrecAnd : FtoF = rule(andq ~ andF)

  def rrecImpies: Rule[Elo, Elo] = rule(
    /* nonRecByNat ~*/ ("->" | "=>") ~ WhiteSpace ~ Formula ~> ((a: Formula, b: Formula) => Implies(a, b))
  )

  def rrecEquiv: Rule[Elo, Elo] =
    rule(("<->" | "<=>") ~ WhiteSpace ~ Formula ~> ((a: Formula, b: Formula) => Equivalent(a, b)))

  def rrecOr: FtoF =
    rule(("or" | "||") ~ WhiteSpace ~ lv1rec ~> ((a: Formula, b: Formula) => Or(a, b)))

  def rrecAnd: FtoF =
    rule(("and" | "&&") ~ WhiteSpace ~ lv1rec ~> ((a: Formula, b: Formula) => And(a, b)))


  def Pred: Rule1[Predicate] = rule { // hax
    Fn ~> (fn => Predicate(PredicateName(fn.name.name), fn.args))
  }

  def Name: Rule1[FunctionName] =
    rule((capture(Parserq.NameCharS ~ zeroOrMore(Parserq.NameChar) ~ !Parserq.NameChar) ~> (s => FunctionName(s))) ~ (WhiteSpace))


  def Fn: Rule1[Function] = rule {
    (
      (Name ~ !ch('(')) ~> ((f: FunctionName) => Function(f, Seq())) |
        (Name ~ ws('(') ~ args ~ ws(')')) ~> ((f: FunctionName, s: Seq[Term]) => Function(f, s))

      )
  }

  def ws(c: Char): Rule0 = rule(c ~ WhiteSpace)

  def args: Rule1[Seq[Term]] = rule {
    (zeroOrMore(Term).separatedBy(ws(',')))
  }


  // only fns, rest later to fix D:
  def Term: Rule1[Term] = rule {
    Fn
  }

  def WhiteSpace: Rule0 = rule(zeroOrMore(WhiteSpaceChar))

}

object Parserq {
  private val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
  private var LPar = CharPredicate("(")
  private var RPar = CharPredicate(")")

  private def NameCharS = CharPredicate.from(_.isUnicodeIdentifierStart)

  private def NameChar = CharPredicate.from(_.isUnicodeIdentifierPart)

  def main(args: Array[String]): Unit = {
    println(Parserq(" eloZiomq").InputLine.run())
    println(Parserq(" eloZiomq)(").InputLine.run())
    println(Parserq(" eloZiŁQomq").InputLine.run())
    println(Parserq(" ~eloZi~ŁQomq").InputLine.run())
    println(Parserq(" eloZiŁQomq ()").InputLine.run())
    println(Parserq(" eloZiŁQomq (").InputLine.run())
    println(Parserq(" eloZiŁQomq (he,qwe(), ddd(a,b, dq, a) )").InputLine.run())
    println(Parserq("a and b and c").InputLine.run())
    val value = Parserq("a and b and c -> d and e").InputLine.run()
    println(value)
    println(Parserq("(a and b and c) -> d and e").InputLine.run())

  }
}