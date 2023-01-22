package pl.wojciechkarpiel.tableaux
package api

import parser.Parser
import lang.Formula
import org.parboiled2.ParserInput

import scala.util.Try

object FormulaParser {

  def parse(input: String): Try[Formula] = Parser.run(input)

  def parse(input: Array[Char]): Try[Formula] = Parser.run(input)

  def parse(input: ParserInput): Try[Formula] = Parser.run(input)
}
