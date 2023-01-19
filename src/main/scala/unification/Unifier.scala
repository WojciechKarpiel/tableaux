package pl.wojciechkarpiel.tableaux
package unification

import lang.Term.Variable
import unification.Unifier.UnificationResult.{UnificationFailure, UnificationSuccess}
import unification.Unifier.UnifierTerm.*

import scala.annotation.{tailrec, targetName}

object Unifier {

  enum UnifierTerm:
    case Tree(name: Variable, branches: Seq[UnifierTerm])
    case Unifiable(id: Variable) extends UnifierTerm

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
      def rec(unificationProblems: Seq[(UnifierTerm, UnifierTerm)]): UnificationResult =
        unificationProblems match
          case (a, b) :: tail => rec(tail) match
            case UnificationFailure => UnificationFailure
            case UnificationSuccess(tailSubstitution) =>
              unify(applySubstitution(a, tailSubstitution), applySubstitution(b, tailSubstitution)) match
                case UnificationFailure => UnificationFailure
                case UnificationSuccess(headSubstitution) =>
                  UnificationSuccess(headSubstitution.concat(tailSubstitution))
          case Nil => UnificationSuccess(Substitution.empty())

      rec(a.branches.zip(b.branches))
    } else UnificationFailure

  private def applySubstitution(term: UnifierTerm, substitution: Substitution): UnifierTerm =
    term match
      case u: Unifiable => substitution.find(u).getOrElse(u)
      case Tree(name, branches) => Tree(name, branches.map(branch => applySubstitution(branch, substitution)))

  private def treeUnifiable(u: Unifiable, t: Tree): UnificationResult =
    if occurs(u, t) then UnificationFailure else UnificationSuccess(Substitution(u, t))

  private def occurs(x: Unifiable, term: UnifierTerm): Boolean = term match
    case u: Unifiable => u == x
    case Tree(_, branches) => branches.exists(branch => occurs(x, branch))
}
