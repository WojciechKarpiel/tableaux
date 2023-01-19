package pl.wojciechkarpiel.tableaux
package parser

import lang.Formula.*
import lang.Term.*
import lang.{Formula, Term}

import org.parboiled2.{Parser => ParboiledParser, *}
import org.parboiled2.support.hlist.{::, HList, HNil}

import scala.annotation.{compileTimeOnly, tailrec}
import scala.collection.immutable
import scala.util.Try

class Parser(val input: ParserInput) extends ParboiledParser {

  import Parser.*

  def InputLine: Rule1[Formula] = rule {
    WhiteSpace ~ Formula ~ EOI
  }

  def Formula: Rule1[Formula] =
  //    rule(nonLeftRecursiveByNature ~ RRECSTUFF.?)
    lv2rec

  def nonLeftRecursiveByNature: Rule1[Formula] = rule(parend | notR /* | frall | ex */ | Pred)

  /*
  \lnot is evaluated first
  ∧ \land and ∨ \lor are evaluated next
  Quantifiers are evaluated next
  → \to is evaluated last.
  */

  def frall: Rule1[Formula] = rule(("∀" | safename("forall")) ~ WhiteSpace ~ variable ~ optDot ~ lv1Arec ~> ((f: NamedVar, b: Formula) =>
    ForAll(f, fixVariable(f, b))))

  def ex: Rule1[Formula] = rule(("∃" | safename("exists")) ~ WhiteSpace ~ variable ~ optDot ~ lv1Arec ~> ((f: NamedVar, b: Formula) =>
    Exists(f, fixVariable(f, b))))

  def optDot: Rule0 = rule(optional(ch('.')) ~ WhiteSpace)


  def variable: Rule1[NamedVar] = rule(Name ~> ((f: FunctionName) => NamedVar(f.name))) // hax

  def lvl0rec: Rule1[Formula] = nonLeftRecursiveByNature

  def lv1rec: Rule1[Formula] = rule(lvl0rec ~ (rrecAnd | rrecOr).?)

  def lv1Arec: Rule1[Formula] = rule((frall | ex) | (lv1rec ~ (rrecAnd | rrecOr).?))


  def lv2rec: Rule1[Formula] = rule(lv1Arec ~ (rrecEquiv | rrecImpies).?)

  def RRECSTUFF: FtoF = rule(
    rrecAnd | rrecOr |
      rrecEquiv | rrecImpies
  )

  def notR: Rule1[Formula] = rule {
    ("!" | "~" | "¬" | safename("not")) ~ WhiteSpace ~ Formula ~> ((f: Formula) => Not(f))
  }

  def parend: Rule1[Formula] = rule(ws('(') ~ Formula ~ ws(')'))


  type Elo = Formula :: HNil //::[Formula, HNil]
  type Elo2 = ::[Formula, ::[Formula, HNil]]

  type FtoF = Rule[Elo, Elo]
  //  def rrecAnd : FtoF = rule(andq ~ andF)

  def rrecImpies: Rule[Elo, Elo] = rule(
    /* nonRecByNat ~*/ ("->" | "=>" | "⇒") ~ WhiteSpace ~ Formula ~> ((a: Formula, b: Formula) => Implies(a, b))
  )

  def rrecEquiv: Rule[Elo, Elo] =
    rule(("<->" | "<=>" | "⇔") ~ WhiteSpace ~ Formula ~> ((a: Formula, b: Formula) => Equivalent(a, b)))

  def rrecOr: FtoF =
    rule((safename("or") | "∨" | "||") ~ WhiteSpace ~ lv1rec ~> ((a: Formula, b: Formula) => Or(a, b)))

  def rrecAnd: FtoF =
    rule((safename("and") | "∧" | "&&") ~ WhiteSpace ~ lv1rec ~> ((a: Formula, b: Formula) => And(a, b)))


  def Pred: Rule1[Predicate] = rule { // hax
    Fn ~> (fn => Predicate(PredicateName(fn.name.name), fn.args))
  }

  def wordBoundary: Rule0 = rule(!Parser.NameChar)

  def Name: Rule1[FunctionName] =
    rule((capture(Parser.NameCharS ~ zeroOrMore(Parser.NameChar) ~ !Parser.NameChar) ~> (s => FunctionName(s))) ~ (WhiteSpace))


  def safename(string: String): Rule0 = rule(string ~ wordBoundary)

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

object Parser {
  private val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
  private var LPar = CharPredicate("(")
  private var RPar = CharPredicate(")")

  private def NameCharS = CharPredicate.from(_.isUnicodeIdentifierStart)

  private def NameChar: CharPredicate = CharPredicate.from(_.isUnicodeIdentifierPart)

  def run(input: ParserInput): Try[Formula] = Parser(input).InputLine.run()


  def fixTerm(v: NamedVar, term: Term): Term = term match
    case variable: Variable => variable
    case f@Function(name, _) => /* fail if args>0 */ if (name.name == v.name) v else f


  // see usage
  def fixVariable(v: NamedVar, formula: Formula): Formula = formula match
    case Equal(a, b) => Equal(fixTerm(v, a), fixTerm(v, b))
    case Predicate(name, args) => Predicate(name, args.map(a => fixTerm(v, a)))
    case Not(formula) => Not(fixVariable(v, formula))
    case f@ForAll(variable, body) => (if variable == v then f /* shadowed */
    else ForAll(variable, fixVariable(v, body)))
    case e@Exists(variable, body) => (if variable == v then e /* shadowed */
    else Exists(variable, fixVariable(v, body)))
    case And(a, b) => And(fixVariable(v, a), fixVariable(v, b))
    case Or(a, b) => Or(fixVariable(v, a), fixVariable(v, b))
    case Equivalent(a, b) => Equivalent(fixVariable(v, a), fixVariable(v, b))
    case Implies(premise, conclusion) => Implies(fixVariable(v, premise), fixVariable(v, conclusion))
}