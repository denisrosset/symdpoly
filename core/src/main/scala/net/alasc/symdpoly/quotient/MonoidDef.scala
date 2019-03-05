package net.alasc.symdpoly
package quotient
import net.alasc.algebra.PermutationAction
import net.alasc.symdpoly.free.{MutablePoly, MutableWord}
import net.alasc.symdpoly.generic.{FreeBasedMono, FreeBasedMonoidDef, FreeBasedPermutation}
import spire.syntax.cfor._

import scala.annotation.tailrec
import net.alasc.bsgs.UnorderedPartitionStabilizer
import net.alasc.finite.Grp
import net.alasc.partitions.Partition
import net.alasc.symdpoly
import net.alasc.symdpoly.Phase
import net.alasc.symdpoly.math.GenPerm
import net.alasc.util._
import cats.instances.vector._
import spire.syntax.group._
import cats.instances.option._
import cats.syntax.traverse._
import shapeless.Witness
/*
case class RewritingRules[F <: free.MonoidDef with Singleton](root: RewritingRules.Node[F])

object RewritingRules {
  sealed trait Node[F <: free.MonoidDef with Singleton]
  case class Branch[F <: free.MonoidDef with Singleton](branches: Map[F#Op, Node[F]]) extends Node[F]
  case class Leaf[F <: free.MonoidDef with Singleton](rhs: F#Monomial) extends Node[F]

  def apply[F <: free.MonoidDef with Singleton:Witness.Aux](rules: (F#Monomial, F#Monomial)*): RewritingRules[F] = {
    assert(rules.forall(_._1.phase.isOne), "Phase must be one for rewriting rules left hand side")
    assert(!rules.exists(_._1.isZero), "The zero monomial cannot be rewritten")
    assert(!rules.exists(_._1.isOne), "The monomial 1 cannot be rewritten")
    def branch(nodeRules: Seq[(F#Monomial, F#Monomial)]): Branch[F] = {
      val branches = nodeRules.groupBy( pair => pair._1.apply(0) ).mapValues { forBranch =>
        forBranch.find(_._1.length == 1) {
          case Some((lhs, rhs)) => Leaf(rhs)
          case None =>
            val discardingOp = forBranch.map {
              case (lhs, rhs) => lhs.data.
            }
        }
        if (forBranch.exists(_._1.length == 1))
        forBranch.map { case () }
      }
    }
    RewritingRules[F](branch(rules))
  }

}
*/
/** Base class for quotient monoids. */
abstract class MonoidDef extends FreeBasedMonoidDef {
  monoidDef =>

  def cyclotomicOrder: Int = Free.cyclotomicOrder

  /** List of substitution rules that provide the normal form. */
  def pairRules: PairRules[Free]

  /** Helper to describe the symmetries that are compatible with the quotient monoid. */
  object Symmetries {

    private[this] val m = Free.cyclotomicOrder
    private[this] val n = Free.nOperators
    private[this] val phases: Vector[Phase] = Vector.tabulate(m)(k => Phase(k, m))
    private[this] def op(i: Int): Free#Op = Free.opFromIndex(i)
    private[this] def monoFromOpIndex(i: Int): FreeBasedMono[Free, Free] = FreeBasedMono(op(i))

    /** Ordered set of all monomials of degree <= 2, with all possible phases. */
    private[this] val monoSet: OrderedSet[FreeBasedMono[Free, Free]] = {
      val monos1: Vector[FreeBasedMono[Free, Free]] = Vector.tabulate(n)(i => monoFromOpIndex(i))
      val monos2: Vector[FreeBasedMono[Free, Free]] = Vector.tabulate(n, n)((i, j) => FreeBasedMono(op(i), op(j)) ).flatten
      val monos: Vector[FreeBasedMono[Free, Free]] = Vector(FreeBasedMono.one[Free, Free]) ++ (monos1 ++ monos2).flatMap(m => phases.map(p => m * p))
      OrderedSet.fromUnique(monos)
    }

    /** Permutation action of free permutations on the set of monomials described by [[monoSet]] . */
    val action = new PermutationAction[FreeBasedPermutation[Free, Free]] {
      def isFaithful: Boolean = true
      def findMovedPoint(g: FreeBasedPermutation[Free, Free]): NNOption = g.genPerm.largestMovedPoint match {
        case NNOption(i) => NNSome(monoSet.indexOf(monoFromOpIndex(i)))
        case _ => NNNone
      }
      def movedPointsUpperBound(g: FreeBasedPermutation[Free, Free]): NNOption = NNSome(monoSet.length - 1)
      def actl(g: FreeBasedPermutation[Free, Free], i: Int): Int = actr(i, g.inverse)
      def actr(i: Int, g: FreeBasedPermutation[Free, Free]): Int = monoSet.indexOf(Free.monoGenPermAction.actr(monoSet(i), g.genPerm)) // TODO: replace
    }

    /** Partition given by equivalent monomials in [[monoSet]]. */
    val partition = {
      val normalForms = monoSet.iterator.map(monoidDef.quotient(_)).toVector
      Partition.fromSeq(normalForms)
    }

  }

  /** Returns the subgroup of a group of permutations on the free variables, such that it is the maximal subgroup
    * compatible with the quotient structure. */
  def groupInQuotient(grp: Grp[FreeBasedPermutation[Free, Free]]): Grp[Permutation] = {
    import net.alasc.perms.default._
    grp.generators.toVector.map(quotient).sequence match {
      case Some(mappedGenerators) => Grp.fromGeneratorsAndOrder(mappedGenerators, grp.order)
      case None => groupInQuotient(grp.unorderedPartitionStabilizer(Symmetries.action, Symmetries.partition))
    }
  }

  /** Translates a group acting on the free variables into a group acting on the equivalence classes on the quotient
    * monoid, assuming that the group is compatible without verification. */
  def groupInQuotientNC(grp: Grp[FreeBasedPermutation[Free, Free]]): Grp[Permutation] = {
    import net.alasc.perms.default._
    Grp.fromGeneratorsAndOrder(grp.generators.map(quotientNC), grp.order)
  }

  /** Symmetry group that preserves the quotient structure, with elements that act by permuting operator variables,
    * possibly applying a phase. */
  lazy val symmetryGroup: Grp[Permutation] = groupInQuotient(Free.symmetryGroup)

  /** Returns the permutation of the quotient monoid equivalence classes that correspond to the free permutation given,
    * without performing sanity checks. */
  def quotientNC(permutation: FreeBasedPermutation[Free, Free]): FreeBasedPermutation[monoidDef.type, Free] =
    new FreeBasedPermutation[monoidDef.type, Free](permutation.genPerm)

  /** Returns the permutation of the quotient monoid equivalence classes that correspond to the given free permutation
    * when it is compatible, or returns None otherwise.
    */
  def quotient(permutation: FreeBasedPermutation[Free, Free]): Option[FreeBasedPermutation[monoidDef.type, Free]] =
    if (UnorderedPartitionStabilizer.partitionInvariantUnder(Symmetries.partition, Symmetries.action, permutation))
      Some(new FreeBasedPermutation[monoidDef.type, Free](permutation.genPerm))
    else None

  /** Returns the representative class of the given free monomial. */
  def quotient(word: FreeBasedMono[Free, Free]): Monomial = {
    val res = word.data.mutableCopy()
    inPlaceNormalForm(res)
    new FreeBasedMono[monoidDef.type, Free](res.setImmutable())
  }

  /** Returns the representative class of the given free polynomial. */
  def quotient(poly: Poly[Free, Free]): Poly[monoidDef.type, Free] =
    if (poly.nTerms == 0) symdpoly.Poly.zero[monoidDef.type, Free] else {
      val res = MutablePoly.empty[Free](poly.nTerms)
      cforRange(0 until poly.nTerms) { i =>
        val newMono = poly.monomialNormalForm(i).mutableCopy()
        inPlaceNormalForm(newMono)
        val newCoeff = poly.coeff(i) * newMono.phase.toCyclo
        newMono.setPhase(Phase.one)
        res.add(newMono.setImmutable(), newCoeff)
      }
      res.immutableCopy[monoidDef.type]
    }

  /** Rewrites the given word in the free variables to its normal form in the quotient monoid.
    * Returns whether any rule application has taken place (even if the rule applications left the word
    * invariant in the end).
    */
  def inPlaceNormalForm(word: MutableWord[Free], start: Int = 0): Boolean =
    if (word.isZero) false else {
      @tailrec def rec(i: Int, n: Int, modified: Boolean): Boolean =
        if (i < 0)
          rec(0, n, modified)
        else if (i >= n - 1) {
          word.length = n
          modified
        } else {
          val tailSize = n - i - 2
          val oi1 = word.indices(i)
          val oi2 = word.indices(i + 1)
          pairRules.rule(oi1, oi2) match {
            case PairRules.RemoveBoth => // discard x_i and x_i+1
              System.arraycopy(word.indices, i + 2, word.indices, i, tailSize) // move back two places the tail elements
              rec(i - 1, n - 2, true) // go back one element to check for possible new substitutions
            case PairRules.KeepFirst =>
              System.arraycopy(word.indices, i + 2, word.indices, i + 1, tailSize) // move back one place the tail elements
              rec(i, n - 1, true)
            case PairRules.Swap =>
              word.swap(i, i + 1) // swap x_i and x_i+1
              rec(i - 1, n, true) // go back one element to check for possible new substitutions
            case PairRules.SetToZero =>
              // the monomial was not zero before
              word.setToZero()
              true
            case PairRules.Preserve => rec(i + 1, n, modified) // both elements are good, move to next
            case PairRules.Custom =>
              val result = pairRules.custom(Free.opFromIndex(oi1) -> Free.opFromIndex(oi2))
              if (result.isZero) {
                word.setToZero()
                true
              } else {
                word *= result.phase // multiply phase
                result.length match {
                  case 0 => // as in PairRules.RemoveBoth
                    System.arraycopy(word.indices, i + 2, word.indices, i, tailSize) // move back two places
                    rec(i - 1, n - 2, true)
                  case 1 =>
                    System.arraycopy(word.indices, i + 2, word.indices, i + 1, tailSize)
                    word.indices(i) = result.data.indices(0)
                    rec(i - 1, n - 1, true)
                  case 2 =>
                    word.indices(i) = result.data.indices(0)
                    word.indices(i + 1) = result.data.indices(1)
                    rec(i - 1, n, true)
                  case _ =>
                    throw new IllegalArgumentException(s"Rules cannot grow the word size")
                }
              }
          }
        }
      rec(start, word.length, false)
    }
}

object MonoidDef {

  type Aux[F <: free.MonoidDef with Singleton] = quotient.MonoidDef { type Free = F }

  /** Constructs a quotient monoid on the given free monoid by substitution rules that apply to pairs of operators. */
  def apply[F <: free.MonoidDef.Aux[F] with Singleton](f: F)(pairSubstitutions: PairSubstitutions[F]): quotient.MonoidDef.Aux[F] =
    new MonoidDef {
      type Free = F
      def Free: F = f
      val pairRules: PairRules[Free] = PairRules(pairSubstitutions)
    }

  /** Constructs the commutative quotient monoid on the given free monoid. */
  def commutative[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](f: F): quotient.MonoidDef.Aux[F] = apply(f: F) {
    case (op1, op2) if (f: F).opIndexMap.indexMap(op1) < (f: F).opIndexMap.indexMap(op2) => op2 * op1
    case (op1, op2) => op1 * op2
  }

}
