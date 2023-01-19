package pl.wojciechkarpiel.tableaux
package tree

import lang.Formula
import lang.Formula.*

final class Tree(val formula: Formula) {

  val rootNode: Node = Node(Not(formula))
}
