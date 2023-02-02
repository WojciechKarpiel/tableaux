package pl.wojciechkarpiel.tableaux
package tree

import lang.Formula.{ForAll, Predicate}
import lang.Term.Unifiable
import lang.{Formula, NormalizedHeadFormula, Term}
import tree.Node.NodeId
import tree.RuleType.Gamma
import unification.Unifier.{Substitution, UnificationResult}
import unification.{UnificationFormulaInterop, Unifier}
import util.Gensym

import scala.annotation.tailrec
import scala.collection.immutable.LazyList
import scala.collection.mutable
import modal.World

final class Node private(val formula: Formula, val parent: Option[Node], val originator: Option[Node], val world: World) {

  def this(formula: Formula, parent: Node, originator: Node, world: World) = this(formula, Some(parent), Some(originator), world)

  val id = new NodeId()

  def isTip: Boolean = children.isEmpty

  def ruleType: RuleType = RuleType(formula)

  def children: Seq[Node] = children_

  private var hasExpanded: Boolean = false

  var closedForFree: Boolean = false

  private var children_ : Seq[Node] = Seq()

  private def addChild(child: Node): Unit = children_ = children_ :+ child

  var originated: Seq[Node] = Seq()

  private var blocked = false

  def canExpand: Boolean = !blocked && (!hasExpanded || RuleType(formula) == Gamma)

  def expand(): Boolean = {
    val willExpand = canExpand
    if willExpand then {
      blocked = true
      hasExpanded = true
      val expansion = Expansion(formula)
      findTips.filterNot(_.closedForFree /* no need to expand closed branches */).foreach { tip =>
        expansion.branches.foreach { newBranch =>
          var currentTip = tip
          newBranch.formulas.foreach { newFormula =>
            val newNode = new Node(newFormula, currentTip, Node.this, world) // TODO wordl dependent on rule
            originated = newNode +: originated
            currentTip.addChild(newNode)
            currentTip = newNode
          }
        }
      }
    }
    willExpand
  }


  def findTips: Seq[Node] = {
    def find(n: Node): Seq[Node] =
      if n.isTip then Seq(n)
      else n.children.flatMap(find)

    find(this)
  }

  def unblock(): Unit = blocked = false

  override def toString: String = {
    def idOpt(nodeOpt: Option[Node]): String = nodeOpt.map(_.id.toString).getOrElse("")

    s"$formula <$id> [${idOpt(parent)}] {${idOpt(originator)}} ..::$world::.."
  }
}

object Node {
  def root(formula: Formula, world: World): Node = new Node(formula, None, None, world)

  case class NodeId private(id: Int) {
    def this() = {
      this(NodeCount)
      NodeCount = NodeCount + 1
    }

    override def toString: String = id.toString
  }

  private var NodeCount = 0
}