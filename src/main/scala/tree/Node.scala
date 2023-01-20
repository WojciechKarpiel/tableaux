package pl.wojciechkarpiel.tableaux
package tree

import lang.Formula
import tree.Node.NodeId
import tree.RuleType.Gamma
import util.Gensym

import scala.collection.mutable

final class Node private(var formula: Formula, val parent: Option[Node], val originator: Option[Node]) {

  def this(formula: Formula, parent: Node, originator: Node) = this(formula, Some(parent), Some(originator))

  val id = new NodeId()

  val originalFormula: Formula = formula

  private val backup = new mutable.Stack[Formula]()

  def pushBackup(): Unit = backup.push(formula)

  def popBakcup(): Unit = formula = backup.pop()

  private var hasExpanded: Boolean = false

  var children: Seq[Node] = Seq()
  var originated: Seq[Node] = Seq()

  var blocked = false

  def canExpand: Boolean = !blocked && (!hasExpanded || RuleType(formula) == Gamma)

  def restoreOriginalFormula(): Unit = formula = originalFormula

  def expand(): Boolean = {
    val willExpand = canExpand
    if willExpand then {
      blocked = true
      hasExpanded = true
      val expansion = Expansion(formula)
      findTips.foreach { tip =>
        expansion.branches.foreach { newBranch =>
          var currentTip = tip
          newBranch.formulas.foreach { newFormula =>
            val newNode = new Node(newFormula, currentTip, Node.this)
            originated = (newNode +: originated)
            currentTip.children = (newNode +: currentTip.children)
            currentTip = newNode
          }
        }
      }
    }
    willExpand
  }

  def isTip: Boolean = children.isEmpty


  def ruleType: RuleType = RuleType(formula)

  def findTips: Seq[Node] = {
    def find(n: Node): Seq[Node] =
      if n.isTip then Seq(n)
      else n.children.flatMap(find)

    find(this)
  }

  override def toString: String =
    s"$formula <$id> [${parent.map(_.id.toString).getOrElse("")}] {${originator.map(_.id.toString).getOrElse("")}}"
}

object Node {
  def root(formula: Formula): Node = new Node(formula, None, None)

  case class NodeId private(id: Int) {
    def this() = {
      this(NodeCount)
      NodeCount = NodeCount + 1
    }

    override def toString: String = id.toString
  }

  private var NodeCount = 0
}