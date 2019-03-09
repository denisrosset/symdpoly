package net.alasc.symdpoly
package generic

import scala.reflect.ClassTag
import cats.Invariant
import shapeless.Witness
import spire.algebra.{Action, Eq, Group, Order}
import net.alasc.algebra.PermutationAction
import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.symdpoly.algebra.Phased
import net.alasc.util._
import syntax.all._
import instances.all._
import spire.syntax.cfor._
import spire.syntax.action._
import spire.syntax.group._
import cats.syntax.invariant._
import cats.syntax.contravariant._
import net.alasc.partitions.Partition
import net.alasc.symdpoly.math.{Phase, PhasedInt}
import net.alasc.perms.default._
import net.alasc.symdpoly.util.OrderedSet

/** A Permutation relabels the operator variables of monomials, possibly with a phase. */
trait Permutation[M <: generic.MonoidDef with Singleton] { self: M#Permutation =>

}

object Permutation {

  class GrpPermutationsOps[
    M <: generic.MonoidDef with Singleton: Witness.Aux
  ](grp: Grp[Permutation[M] with M#Permutation]) {

    def M: M = valueOf[M]

    def allElementsOf(poly: generic.Poly[M])(implicit action: Action[M#Monomial, M#Permutation]): OrderedSet[Mono[M]] = {
      implicit def phasedMono: Phased[M#Monomial] = valueOf[M].monoPhased
      implicit def classTag: ClassTag[M#Monomial] = valueOf[M].monoClassTag
      val monomials: Iterator[M#Monomial] = grp.iterator.flatMap(g => Iterator.tabulate(poly.nTerms)(i => phasedMono.phaseCanonical(action.actr(poly.monomial(i), g))))
      val array = monomials.toArray
      spire.math.Sorting.quickSort(array)(valueOf[M].monoOrder, implicitly)
      new OrderedSet(array.map(_.asInstanceOf[AnyRef]))
    }

    /*
    def leavesInvariant(poly: generic.Poly[M])(implicit action: Action[M#Monomial, M#Permutation]): Grp[M#Permutation] = {
      val monomials = allElementsOf(poly)(action)
      val order = M.cyclotomicOrder
      val monomialsAction = new PermutationAction[M#Permutation] {
        def isFaithful: Boolean = false
        def findMovedPoint(g: Permutation[M]): NNOption = {
          cforRange(0 until monomials.length * order) { i =>
            if (actr(i, g) != i) return NNSome(i)
          }
          NNNone
        }
        def movedPointsUpperBound(g: Permutation[M]): NNOption = NNSome(monomials.length * order - 1)
        def actr(p: Int, g: Permutation[M]): Int = {
          val phase = Phase(p % order, order)
          val index = p / order
          val res = action.actr(monomials(index), g)
          val canonical = res.phaseCanonical
          val newPhase = res.phaseOffset * phase
          val newIndex = monomials.indexOf(canonical)
          newIndex * order + newPhase.numeratorIn(order)
        }
        def actl(g: Permutation[M], p: Int): Int = actr(p, g.inverse)
      }
      val nMonomials = monomials.length
      val coeffSeq = for {
        mono <- monomials.iterator.toVector
        coeff = poly.coeff(mono.normalForm: M#Monomial)
        k <- 0 until order
        phase = Phase(k, order)
      } yield coeff * phase.toCyclo
      val partition = Partition.fromSeq(coeffSeq)
      val stabilizer = grp.orderedPartitionStabilizer(monomialsAction, partition)
      val generators = stabilizer.smallGeneratingSet
      Grp.fromGeneratorsAndOrder(generators, stabilizer.order)
    }*/

  }

  implicit def grpGenericPermutationOps[
    M <: generic.MonoidDef with Singleton: Witness.Aux
  ](grp: Grp[Permutation[M]])(implicit classTag: ClassTag[Permutation[M]],
                              equ: Eq[Permutation[M]],
                              fpab: FaithfulPermutationActionBuilder[Permutation[M]],
                              group: Group[Permutation[M]]): GrpPermutationsOps[M] =
    new GrpPermutationsOps[M](grp)

  def phasedIntAction[
    M <: generic.MonoidDef with Singleton:Witness.Aux
  ](set: OrderedSet[M#Monomial])(implicit group: Group[Permutation[M]],
                                 action: Action[M#Monomial, Permutation[M]]): Action[PhasedInt, Permutation[M]] =
    new Action[PhasedInt, Permutation[M]] {
      def actr(p: PhasedInt, g: Permutation[M]): PhasedInt = {
        implicit def phased: Phased[M#Monomial] = valueOf[M].monoPhased
        implicit def order: Order[M#Monomial] = valueOf[M].monoOrder
        val res = set(p.index) <|+| g
        val newIndex = set.indexOf(res.phaseCanonical)
        val newPhase = p.phase * res.phaseOffset
        PhasedInt(newPhase, newIndex)
      }
      def actl(g: Permutation[M], p: PhasedInt): PhasedInt = actr(p, group.inverse(g))
    }

  implicit def evaluatedMonoAction[
    E <: Evaluator[M] with Singleton: Witness.Aux,
    M <: generic.MonoidDef with Singleton
  ](implicit action: Action[M#Monomial, Permutation[M]]): Action[EvaluatedMono[E, M], Permutation[M]] =
    Invariant[Lambda[P => Action[P, Permutation[M]]]].imap[M#Monomial, EvaluatedMono[E, M]](action)((mono: M#Monomial) => (valueOf[E]: E).apply(mono))(_.normalForm)

}