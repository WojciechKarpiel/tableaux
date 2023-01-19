package pl.wojciechkarpiel.tableaux
package javaApi

import lang.Formula.{Predicate, PredicateName}
import lang.Term
import lang.Term.{Function, FunctionName, NamedVar}

import java.util.List as JList

private object JFormulaCompat {

  def predicate(name: String, args: JList[Term]): Predicate =
    Predicate(PredicateName(name), Conversions.asScala(args));

  def function(name: String, args: JList[Term]): Function =
    Function(FunctionName(NamedVar(name)), Conversions.asScala(args));
}
