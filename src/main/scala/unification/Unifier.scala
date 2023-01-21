package pl.wojciechkarpiel.tableaux
package unification

import lang.Formula.Predicate
import lang.Term
import lang.Term.{NamedVar, Variable, Constant}
import unification.Unifier.UnificationResult.{UnificationFailure, UnificationSuccess}
import unification.Unifier.UnifierTerm.*
import util.MetadataHolder

import scala.annotation.{tailrec, targetName}

object Unifier {

  enum UnifierTerm:
    case Tree(name: Constant, branches: Seq[UnifierTerm], metadata: MetadataHolder) // TODO remove need for metadata
    case Unifiable(id: Term.Unifiable)

  opaque type Substitution = Seq[(Unifiable, UnifierTerm)]
  extension (substitution: Substitution) {
    def find(key: Unifiable): Option[UnifierTerm] = {
      @tailrec
      def findRec(list: Seq[(Unifiable, UnifierTerm)]): Option[UnifierTerm] = list match
        case (headKey, headVal) :: tail => if key == headKey then Some(headVal) else findRec(tail)
        case Nil => None

      findRec(substitution)
    }

    def size: Int = substitution.size

    def isEmpty: Boolean = substitution.isEmpty

    def map[T](f: (Unifiable, UnifierTerm) => T): Seq[T] = substitution.map { case (k, v) => f(k, v) }

    private def concat(other: Substitution): Substitution = substitution ++ other
  }

  object Substitution {
    def empty(): Substitution = Seq()

    def apply(unifiable: Unifiable, term: UnifierTerm): Substitution = Seq((unifiable, term))
  }

  enum UnificationResult:
    case UnificationFailure
    case UnificationSuccess(substitution: Substitution)

    def success: Boolean = this match
      case UnificationSuccess(_) => true
      case UnificationFailure => false

  def unify(a: UnifierTerm, b: UnifierTerm): UnificationResult = (a, b) match
    case (a: Unifiable, b: Unifiable) => UnificationSuccess(if a == b then Substitution.empty() else Substitution(a, b))
    case (a: Tree, b: Tree) => treeTree(a, b)
    case (a: Unifiable, b: Tree) => treeUnifiable(a, b)
    case (a: Tree, b: Unifiable) => treeUnifiable(b, a)

  private def treeTree(a: Tree, b: Tree): UnificationResult =
    if a.name == b.name && a.branches.size == b.branches.size then {
      def rec(unificationProblems: List[(UnifierTerm, UnifierTerm)]): UnificationResult =
        unificationProblems match
          case (a, b) :: tail => rec(tail) match
            case UnificationFailure => UnificationFailure
            case UnificationSuccess(tailSubstitution) =>
              unify(applySubstitution(a, tailSubstitution), applySubstitution(b, tailSubstitution)) match
                case UnificationFailure => UnificationFailure
                case UnificationSuccess(headSubstitution) =>
                  UnificationSuccess(headSubstitution.concat(tailSubstitution))
          case Nil => UnificationSuccess(Substitution.empty())

      rec(a.branches.zip(b.branches).toList)
    } else UnificationFailure

  def applySubstitution(term: UnifierTerm, substitution: Substitution): UnifierTerm =
    term match
      case u: Unifiable => substitution.find(u).getOrElse(u)
      case Tree(name, branches, meta) => Tree(name, branches.map(branch => applySubstitution(branch, substitution)), meta)

  private def treeUnifiable(u: Unifiable, t: Tree): UnificationResult =
    if occurs(u, t) then UnificationFailure else UnificationSuccess(Substitution(u, t))

  private def occurs(x: Unifiable, term: UnifierTerm): Boolean = term match
    case u: Unifiable => u == x
    case Tree(_, branches, _) => branches.exists(branch => occurs(x, branch))
}
