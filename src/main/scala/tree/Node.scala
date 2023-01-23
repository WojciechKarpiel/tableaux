package pl.wojciechkarpiel.tableaux
package tree

import lang.Formula.{ForAll, Predicate}
import lang.Term.Unifiable
import lang.{Formula, NormalizedHeadFormula, Term}
import tree.Node.{NodeId, ShouldNotHappenException}
import tree.RuleType.Gamma
import unification.Unifier.{Substitution, UnificationResult}
import unification.{FormulaInterop, Unifier}
import util.Gensym

import scala.annotation.tailrec
import scala.collection.mutable

final class Node private(var formula: Formula, val parent: Option[Node], val originator: Option[Node]) {

  def this(formula: Formula, parent: Node, originator: Node) = this(formula, Some(parent), Some(originator))

  val id = new NodeId()

  val originalFormula: Formula = formula

  private val backup = new mutable.Stack[Formula]()

  def pushBackup(): Unit = backup.push(formula)

  def popBakcup(): Unit = formula = backup.pop()

  private var hasExpanded: Boolean = false

  var closedForFree: Boolean = false

  var children: Seq[Node] = Seq()
  var originated: Seq[Node] = Seq()

  var blocked = false

  /** applies to nodes whose originator is gamma type, otherwise is none. */
  def substitution(): Option[Term] = {
    def findPredicates(f: Formula): LazyList[Formula.Predicate] = f match
      case p: Formula.Predicate => LazyList(p)
      case Formula.Not(formula) => findPredicates(formula)
      case ForAll(variable, body) => findPredicates(body)
      case Formula.Exists(variable, body) => findPredicates(body)
      case Formula.And(a, b) => findPredicates(a) ++ findPredicates(b)
      case Formula.Or(a, b) => findPredicates(a) ++ findPredicates(b)
      case Formula.Equivalent(a, b) => findPredicates(a) ++ findPredicates(b)
      case Formula.Implies(premise, conclusion) => findPredicates(premise) ++ findPredicates(conclusion)


    originator //.filter (_.ruleType==Gamma)
      .map(_.formula).collect { case fa: ForAll => fa }.map { parentForall =>
      val (unifiable, expanded) = Expansion.gammaExpansion(parentForall)
      val flexiblePreds = findPredicates(expanded)
      val rigidPreds = findPredicates(this.formula)
      val pairs = flexiblePreds.zip(rigidPreds)

      @tailrec
      def findUnified(pairs: LazyList[(Predicate, Predicate)]): Term =
        if pairs.isEmpty then throw new ShouldNotHappenException()
        else {
          val (a, b) = pairs.head
          Unifier.unify(FormulaInterop.apply(a), FormulaInterop.apply(b)) match
            case UnificationResult.UnificationFailure => throw new ShouldNotHappenException()
            case UnificationResult.UnificationSuccess(substitution) =>
              substitution.headOption match
                case Some((uVar, uTerm)) =>
                  val term = FormulaInterop.toLogicTerm(uTerm)
                  if term == unifiable then FormulaInterop.toLogicTerm(uVar) else term
                case None => findUnified(pairs.tail)
        }

      findUnified(pairs)
    }
  }

  def canExpand: Boolean = !blocked && (!hasExpanded || RuleType(formula) == Gamma)

  def restoreOriginalFormula(): Unit = formula = originalFormula

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
            val newNode = new Node(newFormula, currentTip, Node.this)
            originated = newNode +: originated
            currentTip.children = newNode +: currentTip.children
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

  override def toString: String = {
    def idOpt(nodeOpt: Option[Node]): String = nodeOpt.map(_.id.toString).getOrElse("")

    s"$formula <$id> [${idOpt(parent)}] {${idOpt(originator)}} ${substitution().map(t => s"..::$t::..").getOrElse("")}"
  }
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

  class ShouldNotHappenException extends RuntimeException
}