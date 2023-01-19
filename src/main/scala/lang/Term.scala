package pl.wojciechkarpiel.tableaux
package lang

import util.Gensym

sealed trait Term

object Term {

  /**
   * Variables. Any variable symbol is a term.
   */
  sealed trait Variable extends Term

  case class NamedVar(name: String) extends Variable

  final case class Unifiable(gensym: Gensym) extends Variable {
    def this() = this(new Gensym())
  }

  case class InternVar private(gensym: Gensym) extends Variable {
    def this() = this(new Gensym())
  }

  /**
   * Functions. If f is an n-ary function symbol, and t1, ..., tn are terms, then f(t1,...,tn) is a term. In particular, symbols denoting individual constants are nullary function symbols, and thus are terms.
   */
  case class Function(name: FunctionName, args: Seq[Term]) extends Term {
    def arity: Int = args.size

    def isAtom: Boolean = arity == 0
  }

  case class FunctionName(name: String) extends AnyVal
}