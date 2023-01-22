package pl.wojciechkarpiel.tableaux
package api

import api.ConversionsBecauseJavaCannotIntoImportingFormula.given
import lang.Formula.*
import lang.Term.*

import java.util
import java.util.function.Function as JFunction
import scala.annotation.varargs
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

object FormulaBuilder {

  def predicate(name: String): PredicateFormulaBuilder = new PredicateFormulaBuilder(name)

  def and(a: Formula, b: Formula): Formula = And(a, b)

  def or(a: Formula, b: Formula): Formula = Or(a, b)

  def not(formula: Formula): Formula = Not(formula)

  def implies(premise: Formula, conclusion: Formula): Formula = Implies(premise, conclusion)

  def equivalent(a: Formula, b: Formula): Formula = Equivalent(a, b)

  def forAll(variable: String): ForAllFormulaBuilder = new ForAllFormulaBuilder(variable)

  def exists(variable: String): ExistsFormulaBuilder = new ExistsFormulaBuilder(variable)

  def function(functionName: String): FunctionTermBuilder = new FunctionTermBuilder(functionName)

  def constant(name: String): Term = function(name).apply()

  final class PredicateFormulaBuilder private[FormulaBuilder](predicateName: String) {

    @varargs
    def apply(args: Term*): Formula = toInternal(args: _*)

    private def toInternal: PredicateName = PredicateName(predicateName)
  }

  final class FunctionTermBuilder private[FormulaBuilder](functionName: String) {

    @varargs
    def apply(args: Term*): Term = toInternal(args: _*)

    private def toInternal: FunctionName = FunctionName(NamedVar(functionName))
  }

  final class ForAllFormulaBuilder private[FormulaBuilder](variableName: String) {
    private val variable: NamedVar = NamedVar(variableName)

    def apply(body: Term => Formula): Formula = ForAll(variable, body(variable))
  }

  final class ExistsFormulaBuilder private[FormulaBuilder](variableName: String) {
    private val variable: NamedVar = NamedVar(variableName)

    def apply(body: Term => Formula): Formula = Exists(variable, body(variable))
  }
}
