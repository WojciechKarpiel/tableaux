package pl.wojciechkarpiel.tableaux
package javaApi

import pl.wojciechkarpiel.tableaux.lang.Formula.{Predicate, PredicateName}
import pl.wojciechkarpiel.tableaux.lang.Term
import pl.wojciechkarpiel.tableaux.lang.Term.Function
import pl.wojciechkarpiel.tableaux.lang.Term.FunctionName

import java.util.List as JList

private object JFormulaCompat {

  def predicate(name: String, args: JList[Term]): Predicate =
    Predicate(PredicateName(name), Conversions.asScala(args));

  def function(name: String, args: JList[Term]): Function =
    Function(FunctionName(name), Conversions.asScala(args));
}
