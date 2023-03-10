package pl.wojciechkarpiel.tableaux
package lang

import util.{Gensym, Printing}

sealed trait Term

object Term {

  /**
   * Variables. Any variable symbol is a term.
   */
  sealed trait Variable extends Term

  sealed trait Constant extends Variable

  case class NamedVar(name: String) extends Constant {
    override def toString: String = name
  }

  case class InternVar private(gensym: Gensym) extends Constant {
    def this() = this(new Gensym())

    override def toString: String = s"I$gensym"
  }

  final case class Unifiable(gensym: Gensym) extends Variable {
    def this() = this(new Gensym())

    override def toString: String = s"U$gensym"
  }

  /**
   * Functions. If f is an n-ary function symbol, and t1, ..., tn are terms, then f(t1,...,tn) is a term. In particular, symbols denoting individual constants are nullary function symbols, and thus are terms.
   */
  case class Function(name: FunctionName, args: Seq[Term]) extends Term {
    def arity: Int = args.size

    def isAtom: Boolean = arity == 0

    override def toString: String = Printing.printFunctionLike(name.name, args)
  }

  object Function {
    def constant(name: FunctionName): Function = Function(name, Seq())
  }

  case class FunctionName(name: Constant) extends AnyVal {
    override def toString: String = name.toString

    def apply(args: Term*): Function = Function(this, args)
  }
}