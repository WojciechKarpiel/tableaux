package pl.wojciechkarpiel.tableaux
package tree

import lang.Formula.*
import lang.Term.NamedVar
import lang.{Formula, NormalizedHeadFormula, Term}
import modal.{World, WorldManager, WorldManagerImpl}
import parser.Parser
import tree.Node.{NodeId, root}
import tree.RuleType.Gamma
import unification.Unifier.{Substitution, UnificationResult, UnifierTerm}
import unification.{UnificationFormulaInterop, Unifier}

import org.parboiled2.ParseError

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success}

final class Tree(val formula: Formula, debug: Boolean) {
  Node.haxTree = this

  def this(formula: Formula) = this(formula, false)

  def this(formula: String, debug: Boolean) = this(Parser.parseOrThrow(formula), debug)

  def this(formula: String) = this(formula, false)

  private val worldManager = new WorldManagerImpl()

  private val rootNode: Node = Node.root(Not(formula), worldManager.initialWorld)

  /**
   *
   * @return true if anything was expanded
   */
  def expand(skippedRules: Set[RuleType]): Boolean =
    var anyExpansionChange = false

    @tailrec
    def expansionLoop(): Unit =
      val anyChanged = RuleType.values.filterNot(skippedRules.contains).exists(expandAll)
      if anyChanged then
        anyExpansionChange = true
        expansionLoop()

    def unblock(node: Node): Unit =
      node.unblock()
      node.children.foreach(unblock)

    expansionLoop()
    unblock(rootNode)
    anyExpansionChange

  /**
   * returns true if anything was expanded
   */
  private def expandGammaOnce(): Boolean = expand(Set())

  private def expandNonGamma(): Boolean = expand(Set(Gamma))

  /**
   *
   * @return true if any node was expanded
   */
  private def expandAll(ruleType: RuleType): Boolean = {
    var changed = false

    def traverse(n: Node): Unit = {
      if (n.ruleType == ruleType) {
        doDebug {
          if (n.ruleType == Gamma) println("expanding gammma " + n.formula)
        }
        val hasExpanded = n.expand(worldManager)
        if hasExpanded then changed = true
      }
      n.children.foreach(traverse)
    }

    traverse(rootNode)
    changed
  }

  def printTree(): Unit = {
    def loop(n: Node, ident: Int): Unit = {
      print(" ".repeat(ident) + "|")
      println(n)
      n.children.foreach(loop(_, ident + 1))
    }

    loop(rootNode, 0)
  }

  def findTips: Seq[Node] = rootNode.findTips

  private def branchClosingUnifiables(tip: Node): Seq[(UnifierTerm, UnifierTerm)] = {

    def findPredicates(node: Node): Seq[(Predicate, World)] = {
      val newPred: Seq[(Predicate, World)] = node.formula match
        case predicate: Predicate => Seq((predicate, node.world))
        case _ => Seq[(Predicate, World)]()
      newPred ++ node.parent.map(findPredicates).getOrElse(Seq())
    }

    def findNegatedPredicates(node: Node): Seq[(Predicate, World)] = {
      val newPred = node.formula match
        case Not(predicate: Predicate) => Seq((predicate, node.world))
        case _ => Seq()
      newPred ++ node.parent.map(findNegatedPredicates).getOrElse(Seq())
    }

    val preds = findPredicates(tip).distinct
    val negated = findNegatedPredicates(tip).distinct
    preds.map { case (a, b) => (UnificationFormulaInterop.toUnifierTerm(a), b) }.flatMap { case (pred, predW) =>
      negated.filter(_._2 == predW).map(_._1).map(UnificationFormulaInterop.toUnifierTerm).flatMap { negPred =>
        val result = Unifier.unify(pred, negPred)
        result match
          case UnificationResult.UnificationFailure => Seq()
          case UnificationResult.UnificationSuccess(_) => Seq((pred, negPred))
      }
    }
  }

  /**
   * @return substitution that allows for closing all branches of the tree (or None if no such found)
   */
  @tailrec
  def solve(maxGammaExpansions: Int): Option[Substitution] = {
    expandNonGamma()
    val tips = findTips
    val initialCandidates = tips.flatMap { tip =>
      if tip.closedForFree then None
      else {
        val res = branchClosingUnifiables(tip)
        val freeUnifL = freeUnification(res)
        tip.closedForFree |= freeUnifL.isDefined
        if (tip.closedForFree) doDebug(println(s"$tip is closed for free: $freeUnifL"))
        Some(res)
      }
    }
    val candidates = reduceCandidates(initialCandidates)

    val doomedAlready = candidates.exists(_.isEmpty)
    val solvingSubstitution = if !doomedAlready then hardcoreSolve(candidates.map(_.toList).toList) else None
    solvingSubstitution match
      case Some(substitution) =>
        doDebug(println(s"Winning sub: $substitution"))
        Some(substitution)
      case None => if maxGammaExpansions == 0 then {
        doDebug(printTree())
        None
      } else {
        doDebug(println("expanding gammas!!!!!"))
        val worthTryingAgain = expandGammaOnce()
        if worthTryingAgain then solve(maxGammaExpansions - 1) else None
      }
  }

  private def hasFreeUnification(v: Seq[(Unifier.UnifierTerm, Unifier.UnifierTerm)]): Boolean =
    freeUnification(v).isDefined

  private def freeUnification(v: Seq[(Unifier.UnifierTerm, Unifier.UnifierTerm)]): Option[(UnifierTerm, UnifierTerm)] = {
    v.find { case (a, b) => Unifier.unify(a, b) match
      case UnificationResult.UnificationFailure => false
      case UnificationResult.UnificationSuccess(substitution) => substitution.isEmpty
    }
  }

  private def reduceCandidates(value: Seq[Seq[(Unifier.UnifierTerm, Unifier.UnifierTerm)]]): Seq[Seq[(UnifierTerm, UnifierTerm)]] = {

    if value.isEmpty then Seq() else {
      val myStuff = value.head

      if hasFreeUnification(myStuff) then reduceCandidates(value.tail)
      else {
        @tailrec
        def dedup(soFar: List[(UnifierTerm, UnifierTerm)], toDo: Seq[(UnifierTerm, UnifierTerm)]): Seq[(UnifierTerm, UnifierTerm)] = {
          if toDo.isEmpty then soFar else {
            val (a, b) = toDo.head
            if (soFar.contains((a, b)) || soFar.contains((b, a))) dedup(soFar, toDo.tail)
            else dedup(toDo.head :: soFar, toDo.tail)
          }
        }

        dedup(Nil, myStuff) +: reduceCandidates(value.tail)
      }
    }
  }

  /**
   * This is also the hottest inner-loop that runs in exponential time
   *
   * @return working substitution, or none if no solution
   */
  private def hardcoreSolve(candidates: List[List[(Unifier.UnifierTerm, Unifier.UnifierTerm)]]): Option[Substitution] = {


    candidates match {
      case headCandidate :: remainingCandidates =>
        if (hasFreeUnification(headCandidate)) {
          doDebug(println(s"this layer is free (remaining:${remainingCandidates.size})  - continuing"))
          hardcoreSolve(remainingCandidates)
        } else {
          @tailrec
          def loop(branchCandidates: List[(Unifier.UnifierTerm, Unifier.UnifierTerm)]): Option[Substitution] = {
            branchCandidates match
              case (candidateA, candidateB) :: tail =>
                Unifier.unify(candidateA, candidateB) match {
                  case UnificationResult.UnificationFailure => loop(tail)
                  case UnificationResult.UnificationSuccess(substitution) =>
                    hardcoreSolve(remainingCandidates.map(substitute(_, substitution))) match
                      case Some(subRest) => doDebug(println(s"znalazłem $candidateA - $candidateB"))
                        Some(substitution.concat(subRest))
                      case None => loop(tail)
                }
              case Nil => None
          }

          loop(headCandidate)
        }
      case Nil =>
        doDebug(println("koniec"))
        Some(Substitution.empty())
    }
  }

  private def doDebug(code: => Unit): Unit = if debug then code


  private def substitute(
                          candidates: List[(Unifier.UnifierTerm, Unifier.UnifierTerm)],
                          substitution: Unifier.Substitution
                        ): List[(Unifier.UnifierTerm, Unifier.UnifierTerm)] =
    candidates.map {
      case (a, b) => (Unifier.applySubstitution(a, substitution), Unifier.applySubstitution(b, substitution))
    }
}

object Tree {
  def isTautology(formula: String, searchBound: Int): Boolean = solve(formula, searchBound).isDefined

  def isTautology(formula: Formula, searchBound: Int): Boolean = solve(formula, searchBound).isDefined

  def solve(formula: String, searchBound: Int): Option[Substitution] = new Tree(formula).solve(searchBound)

  def solve(formula: Formula, searchBound: Int): Option[Substitution] = new Tree(formula).solve(searchBound)
}