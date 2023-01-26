package pl.wojciechkarpiel.tableaux
package api

import api.ConversionHelpersThisLooksBadButSavesMeTyping.given
import parser.Parser
import tree.Tree

import org.parboiled2.ParserInput

import scala.language.implicitConversions

final class Formula private[api](private[api] val formula: lang.Formula) {

  def isTautology(searchBound: Int): Boolean = Tree.isTautology(formula, searchBound)

  override def toString: String = formula.toString

  override def equals(obj: Any): Boolean = Option(obj).exists(_ match
    case other: Formula => formula == other.formula
    case _ => false
  )

  override def hashCode(): Int = formula.hashCode()
}

object Formula {
  def parse(input: String): Formula = Parser.run(input).get

  def parse(input: Array[Char]): Formula = Parser.run(input).get

  def parse(input: ParserInput): Formula = Parser.run(input).get
}

final class Term private[api](private[api] val term: lang.Term) {
  override def toString: String = term.toString

  override def equals(obj: Any): Boolean = Option(obj).exists(_ match
    case other: Term => term == other.term
    case _ => false
  )

  override def hashCode(): Int = term.hashCode()
}
