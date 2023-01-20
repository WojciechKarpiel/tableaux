package pl.wojciechkarpiel.tableaux
package tree

import lang.Formula.*
import lang.Term.NamedVar
import lang.{Formula, NormalizedHeadFormula, Term}
import parser.Parser
import tree.Node.{NodeId, root}
import tree.RuleType.Gamma
import unification.Unifier.{Substitution, UnificationResult, UnifierTerm}
import unification.{FormulaInterop, Unifier}

import org.parboiled2.ParseError

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success}

final class Tree(val formula: Formula, debug: Boolean) {
  def this(formula: Formula) = this(formula, false)

  def this(formula: String, debug: Boolean) = this({
    Parser.run(formula) match
      case Failure(exception) =>
        println(new Parser(formula).formatError(exception.asInstanceOf[ParseError]))
        throw exception
      case Success(value) => value
  }, debug)

  def this(formula: String) = this(formula, false)

  private val rootNode: Node = Node.root(Not(formula))


  /**
   *
   * @return true if anything was expanded
   */
  def expand(forbidden: Set[RuleType]): Boolean = {
    var anyExpansionChange = false

    @tailrec
    def loop(): Unit = {
      val anyChanged = RuleType.values.filterNot(forbidden.contains).exists(expandAll)
      anyExpansionChange = anyExpansionChange || anyChanged
      if anyChanged then loop()
    }

    loop()

    def unblock(node: Node): Unit = {
      node.blocked = false
      node.children.foreach(unblock)
    }

    unblock(rootNode)
    anyExpansionChange
  }

  /**
   * returns true if anything was expanded
   */
  def expandGammaOnce(): Boolean = expand(Set())

  def expandNonGamma(): Boolean = expand(Set(Gamma))

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
        val bool = n.expand() // shortcirtu D:
        changed = changed || bool
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

  def printLinear(): Unit = {
    findTips.foreach { tip =>
      def up(node: Node): Unit = {
        println(node)
        val parent = node.parent
        parent.foreach(up)
      }

      up(tip)
      println(branchClosingUnifiables(tip))
      println("---------------")
      println("")
    }
  }

  def findTips: Seq[Node] = rootNode.findTips


  private def branchClosingUnifiables(tip: Node): Seq[(UnifierTerm, UnifierTerm)] = {

    def findPredicates(node: Node): Seq[Predicate] = {
      val newPred: Seq[Predicate] = node.formula match
        case predicate: Predicate => Seq(predicate)
        case _ => Seq[Predicate]()
      newPred ++ node.parent.map(findPredicates).getOrElse(Seq())
    }

    def findNegatedPredicates(node: Node): Seq[Predicate] = {
      val newPred = node.formula match
        case Not(predicate: Predicate) => Seq(predicate)
        case _ => Seq()
      newPred ++ node.parent.map(findNegatedPredicates).getOrElse(Seq())
    }

    val preds = findPredicates(tip).distinct
    val negated = findNegatedPredicates(tip).distinct
    preds.map(FormulaInterop(_)).flatMap { pred =>
      negated.map(FormulaInterop(_)).flatMap { negPred =>
        val result = Unifier.unify(pred, negPred)
        result match
          case UnificationResult.UnificationFailure => Seq()
          case UnificationResult.UnificationSuccess(_) => Seq((pred, negPred))
      }
    }
  }

  @tailrec
  def solve(maxGammaExpansions: Int): Boolean = {
    expandNonGamma()
    val tips = findTips
    val cndsq = tips.map(branchClosingUnifiables)
    val cnds = reduceq(cndsq)
    doDebug {
      val q = cndsq.map(_.toVector).toVector
      val ccc = cnds.map(_.toVector).toVector
      val szs = ccc.map(_.size)
      val wasPointless = q.map(_.toSet) == ccc.map(_.toSet)
      println(wasPointless.toString + " " + szs)


    }
    val ok = hardcoreSolve(cnds)
    if ok then true else if maxGammaExpansions == 0 then {
      doDebug {
        printTree()
        printLinear()
      }
      false
    } else {
      doDebug(println("expanding gammas!!!!!"))
      val worthTryingAgain = expandGammaOnce()
      if worthTryingAgain then solve(maxGammaExpansions - 1) else false
    }
  }

  private def hasFreeUnif(v: Seq[(Unifier.UnifierTerm, Unifier.UnifierTerm)]): Boolean = {
    v.exists { case (a, b) => Unifier.unify(a, b) match
      case UnificationResult.UnificationFailure => false
      case UnificationResult.UnificationSuccess(substitution) => substitution.isEmpty
    }
  }

  private def reduceq(value: Seq[Seq[(Unifier.UnifierTerm, Unifier.UnifierTerm)]]): Seq[Seq[(UnifierTerm, UnifierTerm)]] = {

    if value.isEmpty then Seq() else {
      val myStuff = value.head

      val hasContantUnif = hasFreeUnif(myStuff)
      if (hasContantUnif) reduceq(value.tail) // only tail
      else {
        @tailrec
        def dedup(soFar: List[(UnifierTerm, UnifierTerm)], toDo: Seq[(UnifierTerm, UnifierTerm)]): Seq[(UnifierTerm, UnifierTerm)] = {
          if toDo.isEmpty then soFar else {
            val (a, b) = toDo.head
            if (soFar.contains((a, b)) || soFar.contains((b, a))) dedup(soFar, toDo.tail)
            else dedup(toDo.head :: soFar, toDo.tail)
          }
        }

        val dd = dedup(Nil, myStuff)
        dd +: reduceq(value.tail)
      }
    }
  }

  /**
   * This is v bad to fix later
   *
   * @return true if ok
   */
  private def hardcoreSolve(candidates: Seq[Seq[(Unifier.UnifierTerm, Unifier.UnifierTerm)]]): Boolean =
    if candidates.isEmpty then {
      doDebug(println("koniec"))
      true
    } else {
      val myPart = candidates.head


      @tailrec
      def loop(myC: Seq[(Unifier.UnifierTerm, Unifier.UnifierTerm)]): Boolean = {
        if (myC.isEmpty) false //no candidates :(
        else {
          val (candA, candB) = myC.head
          Unifier.unify(candA, candB) match {
            case UnificationResult.UnificationFailure => loop(myC.tail)
            case UnificationResult.UnificationSuccess(substitution) =>
              backupFormulas(rootNode)
              propc(rootNode, substitution)
              val rest = candidates.tail.map(propcHHH(_, substitution))

              val ok = hardcoreSolve(rest)
              if ok then {
                doDebug(println(s"znalazłem $candA - $candB"))
                true
              } //solution found
              else {
                revertlol(rootNode)
                loop(myC.tail)
              }
          }
        }
      }

      if (hasFreeUnif(myPart)) { // ta optymalizacja jest potężna!!!
        val rest = candidates.tail
        doDebug(println(s"this layer is free (remaining:${rest.size})  - continuing"))
        hardcoreSolve(candidates.tail)
      } else loop(myPart)
    }

  private def doDebug(code: => Unit): Unit = if debug then code

  // todo ohyda
  private def backupFormulas(node: Node): Unit = {
    node.pushBackup()
    node.children.foreach(backupFormulas)
  }

  private def revertlol(node: Node): Unit = {
    node.popBakcup()
    node.children.foreach(revertlol)
  }

  private def propc(node: Node, sub: Substitution): Unit = {
    val s = FormulaInterop.fixSub(sub)

    def lp(n: Node): Unit = {
      n.formula = subFormula(n.formula, s)
      n.children.foreach(lp)
    }

    lp(node)
  }

  private def subFormula(formula: Formula, m: Map[Term.Unifiable, Term]) = {
    def lpT(t: Term): Term = {
      t match
        case Term.NamedVar(name) => Term.NamedVar(name)
        case u: Term.Unifiable => m.getOrElse(u, u)
        case i: Term.InternVar => i
        case Term.Function(name, args) => Term.Function(name, args.map(lpT))
    }

    def lpF(f: Formula): Formula = {
      f match
        case Predicate(name, args) => Predicate(name, args.map(lpT))
        case Not(formula) => Not(lpF(formula))
        case ForAll(variable, body) => ForAll(variable, lpF(body))
        case Exists(variable, body) => Exists(variable, lpF(body))
        case And(a, b) => And(lpF(a), lpF(b))
        case Or(a, b) => Or(lpF(a), lpF(b))
        case Equivalent(a, b) => Equivalent(lpF(a), lpF(b))
        case Implies(premise, conclusion) => Implies(lpF(premise), lpF(conclusion))
    }

    lpF(formula)
  }

  private def propcHHH(value: Seq[(Unifier.UnifierTerm, Unifier.UnifierTerm)], substitution: Unifier.Substitution):
  Seq[(Unifier.UnifierTerm, Unifier.UnifierTerm)] = {
    //  Unifier.applySubstitution
    value.map {
      case (a, b) => (Unifier.applySubstitution(a, substitution), Unifier.applySubstitution(b, substitution))
    }
  }

  def findNode(id: Int): Option[Node] = {
    def lp(n: Node): Option[Node] = {
      if (n.id.id == id) Some(n)
      else n.children.flatMap(lp).headOption
    }

    lp(rootNode)
  }
}

object Tree {
  def isTautology(formula: String, searchBound: Int): Boolean = new Tree(formula).solve(searchBound)

  def isTautology(formula: Formula, searchBound: Int): Boolean = new Tree(formula).solve(searchBound)
}