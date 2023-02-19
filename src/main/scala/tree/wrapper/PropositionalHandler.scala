package pl.wojciechkarpiel.tableaux
package tree.wrapper

import lang.Formula
import lang.Formula.*
import parser.Parser
import tree.wrapper.PropositionalHandler.PropositionalResult.*
import tree.{Node, Tree}
import util.LogicType

/**
 * Handles the simple case of propositional logic, where being tautology is a decidable problem
 */
object PropositionalHandler:
  def handle(formula: String): Either[StrongerLogic.type, PropositionalResult] = handle(Parser.parseOrThrow(formula))

  def handle(formula: Formula): Either[StrongerLogic.type, PropositionalResult] =
    LogicType.ofFormula(formula) match
      case LogicType.Propositional => Right(unsafeHandle(formula))
      case LogicType.FirstOrder => Left(StrongerLogic)
      case LogicType.Modal => Left(StrongerLogic)

  private def unsafeHandle(propositionalFormula: Formula): PropositionalResult =
    val tautologyTree = Tree(propositionalFormula)
    val negatedTautologyTree = Tree(Not(propositionalFormula))
    if tautologyTree.solve(0).isDefined then Tautology(tautologyTree)
    else if negatedTautologyTree.solve(0).isDefined then NegatedTautology(negatedTautologyTree)
    else Neither(counterExampleForPropositionalFormula(negatedTautologyTree), counterExampleForPropositionalFormula(tautologyTree))

  /**
   * Call only after solution was attempted, only for propositional formulas,
   * and only when formula is neither a tautology nor negated tautology
   *
   * Otherwise will return nonsense
   */
  private def counterExampleForPropositionalFormula(tree: Tree): Map[Predicate, Boolean] =
    def predicateValuations(node: Option[Node]): Map[Predicate, Boolean] =
      node.map(node => node.formula match
        case p: Predicate => Map(p -> true) ++ predicateValuations(node.parent)
        case Not(p: Predicate) => Map(p -> false) ++ predicateValuations(node.parent)
        case _ => predicateValuations(node.parent)
      ).getOrElse(Map())

    predicateValuations(tree.findTips.find(tip => tree.branchClosingUnifiables(tip).isEmpty))

  enum PropositionalResult:
    case Tautology(proof: Tree)
    case NegatedTautology(proof: Tree)
    case Neither(trueValuation: Map[Predicate, Boolean], falseValuation: Map[Predicate, Boolean])

  case object StrongerLogic

