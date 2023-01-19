package pl.wojciechkarpiel.tableaux
package util

import util.Gensym.Count

final case class Gensym private(id: Int) {
  def this() = {
    this(Count)
    Count += 1
  }

  override def toString: String = s"G$id"
}

object Gensym {
  private var Count = 0

  def apply() = new Gensym()
}
