package pl.wojciechkarpiel.tableaux
package util

import lang.Formula
import lang.Formula.*
import lang.Term
import lang.Term.*

object Printing {
  def printFunctionLike(funName: Constant, args: Seq[Term]): String = {
    val sb = new StringBuilder()
    sb.append(funName)
    if (args.nonEmpty) {
      sb.append('(')
      sb.append(args.foldLeft("")((acc, nev) => acc + (if acc != "" then ", " else "") + nev))
      sb.append(')')
    }
    sb.toString()
  }

  def toString(formula: Formula): String = formula match
    case Predicate(name, args) => printFunctionLike(name.name, args)
    case Not(formula) => "(¬" + formula + ")"
    case ForAll(variable, body) => "∀" + variable + ".(" + body + ")"
    case Exists(variable, body) => "∃" + variable + ".(" + body + ")"
    case And(a, b) => "(" + a + " ∧ " + b + ")"
    case Or(a, b) => "(" + a + " ∨ " + b + ")"
    case Equivalent(a, b) => "(" + a + " ⇔ " + b + ")"
    case Implies(premise, conclusion) => s"($premise ⇒ $conclusion)"
    case Necessarily(formula) => s"□($formula)"
    case Possibly(formula) => s"◇($formula)"

}
