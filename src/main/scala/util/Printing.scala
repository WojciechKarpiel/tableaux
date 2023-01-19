package pl.wojciechkarpiel.tableaux
package util

import lang.Term

object Printing {
  def printFunctionLike(funName: String, args: Seq[Term]): String = {
    val sb = new StringBuilder()
    sb.append(funName)
    if (args.nonEmpty) {
      sb.append('(')
      sb.append(args.foldLeft("")((acc, nev) => acc + (if acc != "" then ", " else "") + nev))
      sb.append(')')
    }
    sb.toString()
  }


}
