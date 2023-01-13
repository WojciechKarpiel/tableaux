package pl.wojciechkarpiel.tableaux
package util

import util.Gensym.Count

case class Gensym private(id: Int)

object Gensym {
  def apply(): Gensym = {
    Count += 1
    Gensym(Count)
  }

  private var Count = 0
}
