package pl.wojciechkarpiel.tableaux
package javaApi

import lang.Formula
import parser.Parser

import scala.util.Try

private object JParser {

  def parseTry(input: String): Try[Formula] = Parser.run(input)

  def parseTry(input: Array[Char]): Try[Formula] = Parser.run(input)
}
