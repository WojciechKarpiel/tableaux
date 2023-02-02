package pl.wojciechkarpiel.tableaux
package modal

import util.Gensym

final case class World private(id: Gensym) {
  def this() = this(new Gensym())

  override def toString: String = s"W($id)"
}
