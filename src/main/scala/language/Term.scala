package pl.wojciechkarpiel.tableaux
package language

import util.Gensym

sealed trait Term

object Term {

  /**
   * Variables. Any variable symbol is a term.
   */
  sealed trait Variable extends Term

  case class NamedVar(name: String) extends Variable

  case class Unifiable private(gensym: Gensym) extends Variable

  object Unifiable {
    def apply(): Unifiable = new Unifiable(Gensym())
  }

  case class InternVar private(gensym: Gensym) extends Variable

  object InternVar {
    def apply(): InternVar = new InternVar(Gensym())
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