package pl.wojciechkarpiel.tableaux
package util

import lang.Term.NamedVar
import lang.Term.Function

object Exceptions:
  class TableauxException(message:String) extends RuntimeException(message)

  final case class ClashingVariableAndFunctionName(function:Function, variable:NamedVar)
    extends TableauxException(s"${variable.name} used as a function ($function) name " +
      s"and and a variable ($variable)")

  // TODO upewnić się, że nazwy Predykatów i Zmiennych się nie pokrywają