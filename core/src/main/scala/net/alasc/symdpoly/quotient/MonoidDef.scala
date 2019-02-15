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

abstract class MonoidDef extends FreeBasedMonoidDef {
  monoidDef =>

  def cyclotomicOrder: Int = Free.cyclotomicOrder

  def pairRules: PairRules[Free]

  lazy val (action, partition) = {
    val m = Free.cyclotomicOrder
    val n = Free.nOperators
    def op(i: Int): Free#Op = Free.opFromIndex(i)
    def monoFromOpIndex(i: Int): FreeBasedMono[Free, Free] = FreeBasedMono(op(i))
    val phases: Vector[Phase] = Vector.tabulate(m)(k => Phase(k, m))
    val monos1: Vector[FreeBasedMono[Free, Free]] = Vector.tabulate(n)(i => monoFromOpIndex(i))
    val monos2: Vector[FreeBasedMono[Free, Free]] = Vector.tabulate(n, n)((i, j) => FreeBasedMono(op(i), op(j)) ).flatten
    val monos: Vector[FreeBasedMono[Free, Free]] = Vector(FreeBasedMono.one[Free, Free]) ++ (monos1 ++ monos2).flatMap(m => phases.map(p => m * p))
    val monoSet: OrderedSet[FreeBasedMono[Free, Free]] = OrderedSet.fromUnique(monos)
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
    val normalForms = monoSet.iterator.map(monoidDef.quotient(_)).toVector
    val partition = Partition.fromSeq(normalForms)
    (action, partition)
  }

  def groupInQuotientNC(grp: Grp[FreeBasedPermutation[Free, Free]]): Grp[Permutation] = {
    import net.alasc.perms.default._
    Grp.fromGeneratorsAndOrder(grp.generators.map(quotientNC), grp.order)
  }

  def groupInQuotient(grp: Grp[FreeBasedPermutation[Free, Free]]): Grp[Permutation] = {
    import net.alasc.perms.default._
    grp.generators.toVector.map(quotient).sequence match {
      case Some(mappedGenerators) => Grp.fromGeneratorsAndOrder(mappedGenerators, grp.order)
      case None => groupInQuotient(grp.unorderedPartitionStabilizer(action, partition))
    }
  }

  lazy val symmetryGroup: Grp[Permutation] = groupInQuotient(Free.symmetryGroup)

  def quotientNC(permutation: FreeBasedPermutation[Free, Free]): FreeBasedPermutation[monoidDef.type, Free] =
    new FreeBasedPermutation[monoidDef.type, Free](permutation.genPerm)

  def quotient(permutation: FreeBasedPermutation[Free, Free]): Option[FreeBasedPermutation[monoidDef.type, Free]] =
    if (UnorderedPartitionStabilizer.partitionInvariantUnder(partition, action, permutation)) Some(new FreeBasedPermutation[monoidDef.type, Free](permutation.genPerm)) else None

  def quotient(word: FreeBasedMono[Free, Free]): Monomial = {
    val res = word.data.mutableCopy()
    inPlaceNormalForm(res)
    new FreeBasedMono[monoidDef.type, Free](res.setImmutable())
  }

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
              Array.copy(word.indices, i + 2, word.indices, i, tailSize) // move back two places the tail elements
              rec(i - 1, n - 2, true) // go back one element to check for possible new substitutions
            case PairRules.KeepFirst =>
              Array.copy(word.indices, i + 2, word.indices, i + 1, tailSize) // move back one place the tail elements
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
                    Array.copy(word.indices, i + 2, word.indices, i, tailSize) // move back two places
                    rec(i - 1, n - 2, true)
                  case 1 =>
                    Array.copy(word.indices, i + 2, word.indices, i + 1, tailSize)
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
  def apply[F <: free.MonoidDef.Aux[F] with Singleton](f: F)(pairSubstitutions: PairSubstitutions[F]): quotient.MonoidDef.Aux[F] =
    new MonoidDef {
      type Free = F
      def Free: F = f
      val pairRules: PairRules[Free] = PairRules(pairSubstitutions)
    }
  type Aux[F <: free.MonoidDef with Singleton] = quotient.MonoidDef { type Free = F }

}
