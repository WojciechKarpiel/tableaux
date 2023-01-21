package pl.wojciechkarpiel.tableaux
package util

import lang.Formula.PredicateName
import lang.Term.{FunctionName, NamedVar}

final case class Identifier(name: String) extends AnyVal {
  def toVariable: NamedVar = NamedVar(name)

  def toFunctionName: FunctionName = FunctionName(toVariable)

  def toPredicateName: PredicateName = PredicateName(name)
}
