package pl.wojciechkarpiel.tableaux
package parser

import lang.Formula.*
import lang.Term.*
import lang.{Formula, Term}
import util.Identifier

import org.parboiled2.support.hlist.{::, HList, HNil}
import org.parboiled2.{Parser as ParboiledParser, *}
import pl.wojciechkarpiel.tableaux.lang

import scala.annotation.{compileTimeOnly, tailrec}
import scala.collection.immutable
import scala.util.{Failure, Success, Try}

/**
 * The traditional language of logic is left-recursive, and PEG grammars cannot into direct left-recursion,
 * so the grammar needed to be adjusted to make the left-recursion indirect.
 * That's why infix operators (and, or, implies, equivalent) seem to start parsing in the middle of a formula
 * (see [[andRight]], [[orRight]], [[impliesRight]], [[equivalentRight]]).
 * TBH I blindly decided to experiment with Parboiled before realizing that PEG parsers cannot into left-recursion,
 * but Parboiled turned out to be so nice and easy-to-use that I'm glad I haven't done a proper research first.
 * Seriously, props to Parboiled authors!
 *
 * To support operator priority, there are "levels" of parsing.
 * I think I first saw this trick in Coq's LTac2 parser. IIRC Coq's parser used Menhir, which
 * parses LR(1) grammars, but the trick works in both grammar types. Levels:
 * 0. [[level0]]: not, raw predicates
 * 1. [[level1]]: and, or
 * 2. [[level2]]: modal operators, quantifiers
 * 3. [[level3]]: implies, equivalent
 */
class Parser(val input: ParserInput) extends ParboiledParser {

  import Parser.*

  // Public API
  def runParser(): Try[Formula] = RootRule.run()

  // Types
  private type HFormula = Formula :: HNil
  private type FormulaToFormula = Rule[HFormula, HFormula]
  private type RFormula = Rule1[Formula]

  // Root rule
  private def RootRule: RFormula = rule(whiteSpace ~ Formula ~ EOI)

  private def Formula: RFormula = level3


  // level 0
  private def level0: RFormula = rule(parenthesesFormula | not | predicate)

  private def parenthesesFormula: RFormula = rule(whiteSpaced('(') ~ Formula ~ whiteSpaced(')'))

  private def not: RFormula =
    rule(("!" | "~" | "¬" | wholeWord("not")) ~ whiteSpace ~ level0 ~> ((f: Formula) => Not(f)))

  private def predicate: Rule1[Predicate] = rule(predicateWithoutArgs | predicateWithArgs)

  private def predicateWithoutArgs: Rule1[Predicate] = rule(
    (name ~ whiteSpaced('(') ~ args ~ whiteSpaced(')')) ~>
      ((f: Identifier, args: Seq[Term]) => Predicate(f.toPredicateName, args))
  )

  private def predicateWithArgs: Rule1[Predicate] =
    rule((name ~ !ch('(')) ~> ((f: Identifier) => Predicate(f.toPredicateName, Seq())))


  // level 1
  private def level1: RFormula = rule(level0 ~ (andRight | orRight).?)

  private def orRight: FormulaToFormula =
    rule((wholeWord("or") | "∨" | "||") ~ whiteSpace ~ level2 ~> ((a: Formula, b: Formula) => Or(a, b)))

  private def andRight: FormulaToFormula =
    rule((wholeWord("and") | "∧" | "&&") ~ whiteSpace ~ level2 ~> ((a: Formula, b: Formula) => And(a, b)))


  // level 2
  private def level2: RFormula = rule((necessarily | possibly | forAll | exists) | (level1 ~ (andRight | orRight).?))

  private def forAll: RFormula = rule(
    ("∀" | wholeWord("forall")) ~ whiteSpace ~ variable ~ optionalDot ~ level2 ~>
      ((variable: NamedVar, body: Formula) => ForAll(variable, fixScopedBody(variable, body)))
  )

  private def exists: RFormula = rule(
    ("∃" | wholeWord("exists")) ~ whiteSpace ~ variable ~ optionalDot ~ level2 ~>
      ((variable: NamedVar, body: Formula) => Exists(variable, fixScopedBody(variable, body)))
  )

  private def necessarily: RFormula = rule(("□" | "[]") ~ whiteSpace ~ level2 ~> ((formula: Formula) => Necessarily(formula)))

  private def possibly: RFormula = rule(("◇" | "<>") ~ whiteSpace ~ level2 ~> ((formula: Formula) => Possibly(formula)))

  // level 3
  private def level3: RFormula = rule(level2 ~ (equivalentRight | impliesRight).?)

  private def impliesRight: FormulaToFormula =
    rule(("->" | "=>" | "⇒" | "→") ~ whiteSpace ~ Formula ~> ((a: Formula, b: Formula) => Implies(a, b)))

  private def equivalentRight: FormulaToFormula =
    rule(("<->" | "<=>" | "⇔") ~ whiteSpace ~ Formula ~> ((a: Formula, b: Formula) => Equivalent(a, b)))


  // terms

  /**
   * we only parse functions, and then turn them into Variables by [[fixScopedBody]] and [[fixScopedTerm]]
   */
  private def term: Rule1[Term] = rule(function)

  private def args: Rule1[Seq[Term]] = rule(zeroOrMore(term).separatedBy(whiteSpaced(',')))

  private def function: Rule1[Function] = rule( // hacky reuse Predicate rule
    predicate ~> ((fn: Predicate) => Function(FunctionName(fn.name.name), fn.args))
  )

  private def variable: Rule1[NamedVar] = rule(name ~> ((f: Identifier) => f.toVariable))


  // helpers
  private def wordBoundary: Rule0 = rule(!Parser.IdentifierChar)

  private def name: Rule1[Identifier] = rule(
    capture(Parser.IdentifierStart ~ zeroOrMore(Parser.IdentifierChar) ~ !Parser.IdentifierChar) ~>
      ((s: String) => Identifier(s)) ~ whiteSpace
  )

  private def wholeWord(word: String): Rule0 = rule(word ~ wordBoundary)

  private def whiteSpaced(c: Char): Rule0 = rule(c ~ whiteSpace)

  private def whiteSpace: Rule0 = rule(zeroOrMore(CharPredicate(" \n\r\t\f")))

  private def optionalDot: Rule0 = rule(optional(ch('.')) ~ whiteSpace)
}

object Parser {
  def run(input: ParserInput): Try[Formula] = Parser(input).runParser()

  private def IdentifierStart = CharPredicate.from(c => c.isUnicodeIdentifierStart || c.isDigit /* allow e.g. '0' */)

  private def IdentifierChar: CharPredicate = CharPredicate.from(_.isUnicodeIdentifierPart)

  def parseOrThrow(formula: String): Formula = run(formula) match
    case Failure(parseError: ParseError) =>
      println(new Parser(formula).formatError(parseError))
      throw parseError
    case Failure(exception) => throw exception
    case Success(parsedFormula) => parsedFormula

  private def fixScopedTerm(v: NamedVar, term: Term): Term = term match
    case variable: Variable => variable
    case f@Function(name, _) => // TODO fail if args>0
      if (name.name == v) v else f.copy(args = f.args.map(fixScopedTerm(v, _)))

  private def fixScopedBody(v: NamedVar, formula: Formula): Formula = formula match
    case Predicate(name, args) => Predicate(name, args.map(a => fixScopedTerm(v, a)))
    case Not(formula) => Not(fixScopedBody(v, formula))
    case f@ForAll(variable, body) => if variable == v then f else ForAll(variable, fixScopedBody(v, body))
    case e@Exists(variable, body) => if variable == v then e else Exists(variable, fixScopedBody(v, body))
    case And(a, b) => And(fixScopedBody(v, a), fixScopedBody(v, b))
    case Or(a, b) => Or(fixScopedBody(v, a), fixScopedBody(v, b))
    case Equivalent(a, b) => Equivalent(fixScopedBody(v, a), fixScopedBody(v, b))
    case Implies(premise, conclusion) => Implies(fixScopedBody(v, premise), fixScopedBody(v, conclusion))
    case Necessarily(formula) => Necessarily(fixScopedBody(v, formula))
    case Possibly(formula) => Possibly(fixScopedBody(v, formula))
}