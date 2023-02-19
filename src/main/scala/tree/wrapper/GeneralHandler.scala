package pl.wojciechkarpiel.tableaux
package tree.wrapper

import lang.Formula
import lang.Formula.*
import parser.Parser
import tree.wrapper.PropositionalHandler.{PropositionalResult, StrongerLogic}
import tree.wrapper.Result.{DontKnow, NegatedTautology, Tautology}
import tree.{Node, Tree}
import unification.Unifier.Substitution
import util.LogicType

object GeneralHandler:

  def handle(formula: String, searchBound: Int): Result = handle(Parser.parseOrThrow(formula), searchBound)

  def handle(formula: Formula, searchBound: Int): Result =
    PropositionalHandler.handle(formula) match
      case Right(propositional: PropositionalHandler.PropositionalResult) => Result(propositional)
      case Left(StrongerLogic) =>
        val tautologyTree = new Tree(formula)
        tautologyTree.solve(searchBound) match
          case Some(_) => Tautology(tautologyTree)
          case None =>
            val negatedTautologyTree = new Tree(Not(formula))
            negatedTautologyTree.solve(searchBound) match
              case Some(_) => NegatedTautology(negatedTautologyTree)
              case None => DontKnow(searchBound, tautologyTree, negatedTautologyTree)


