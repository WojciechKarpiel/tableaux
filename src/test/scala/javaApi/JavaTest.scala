package pl.wojciechkarpiel.tableaux
package javaApi

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.parboiled2.ParseError
import lang.Formula.*
import lang.Term.*

import java.util
import java.util.List

class JavaTest extends AnyFlatSpec with should.Matchers {

  "Conversions" should "go both ways" in {
    val originalJavaList: util.List[Integer] = util.List.of(1, 2, 3, 4)
    val scalaList: Seq[Integer] = javaApi.Conversions.asScala(originalJavaList)
    val javaAgain: util.List[Integer] = javaApi.Conversions.asJava(scalaList)
    javaAgain should be(originalJavaList)
  }

  "Parser" should "work from within Java" in {
    javaApi.Parser.parse("a and b") should be
    And(Predicate.constant(PredicateName("a")), Predicate.constant(PredicateName("b")))
    assertThrows[ParseError](javaApi.Parser.parse("a and (b"))
  }
}