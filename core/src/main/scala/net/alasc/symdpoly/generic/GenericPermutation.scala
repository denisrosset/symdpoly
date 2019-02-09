package net.alasc.symdpoly
package generic

import scala.reflect.ClassTag

import cats.{Contravariant, Invariant}
import shapeless.Witness
import cats.syntax.invariant._
import cats.syntax.contravariant._

import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.symdpoly.math.{GenPerm, GenPermFaithfulPermutationAction, PhasedInt, Phases}
import net.alasc.symdpoly.{Phase, free, valueOf}
import net.alasc.symdpoly.algebra.Instances._
import cats.instances.eq._
import spire.syntax.group._
import cats.instances.invariant._
import spire.algebra.{Action, Eq, Group, Order}
import spire.syntax.cfor._
import spire.syntax.action._
import spire.syntax.group._

import cyclo.Cyclo

import net.alasc.perms.default._
import net.alasc.algebra.PermutationAction
import net.alasc.partitions.Partition
import net.alasc.perms.Perm
import net.alasc.symdpoly
import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.algebra.Phased.syntax._
import net.alasc.symdpoly.evaluation._
import net.alasc.symdpoly.evaluation.Symmetries.{allEvaluatedMonomials, momentSetAction}
import net.alasc.symdpoly.quotient.QuotientPermutation
import net.alasc.util.{NNNone, NNOption, NNSome}

trait GenericPermutation[M <: generic.MonoidDef with Singleton]

object GenericPermutation {

  implicit def evaluatedMonoAction[
  E <: Evaluator2[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton
  ](implicit action: Action[M#Monomial, GenericPermutation[M]]): Action[EvaluatedMono2[E, M], GenericPermutation[M]] =
    Invariant[Lambda[P => Action[P, GenericPermutation[M]]]].imap[M#Monomial, EvaluatedMono2[E, M]](action)((mono: M#Monomial) => (valueOf[E]: E).apply(mono))(_.normalForm)

  implicit def grpGenericPermutationOps[M <: generic.MonoidDef with Singleton: Witness.Aux](grp: Grp[GenericPermutation[M]])
                                                                                           (implicit classTag: ClassTag[GenericPermutation[M]], equ: Eq[GenericPermutation[M]], fpab: FaithfulPermutationActionBuilder[GenericPermutation[M]], group: Group[GenericPermutation[M]]): GrpGenericPermutationOps[M, GenericPermutation[M]] =
    new GrpGenericPermutationOps[M, GenericPermutation[M]](grp)

  def phasedIntAction[M <: generic.MonoidDef with Singleton:Witness.Aux](set: OrderedSet[M#Monomial])(implicit group: Group[GenericPermutation[M]], action: Action[M#Monomial, GenericPermutation[M]]): Action[PhasedInt, GenericPermutation[M]] =
    new Action[PhasedInt, GenericPermutation[M]] {
      def actr(p: PhasedInt, g: GenericPermutation[M]): PhasedInt = {
        implicit def phased: Phased[M#Monomial] = valueOf[M].monoPhased
        implicit def order: Order[M#Monomial] = valueOf[M].monoOrder
        val res = set(p.index) <|+| g
        val newIndex = set.indexOf(res.phaseCanonical)
        val newPhase = p.phase * res.phaseOffset
        PhasedInt(newPhase, newIndex)
      }
      def actl(g: GenericPermutation[M], p: PhasedInt): PhasedInt = actr(p, group.inverse(g))
    }

  class GrpGenericPermutationOps[
  M <: generic.MonoidDef with Singleton: Witness.Aux,
  G <: GenericPermutation[M]:ClassTag:Eq:FaithfulPermutationActionBuilder:Group
  ](grp: Grp[G]) {

    def M: M = valueOf[M]

    def allElementsOf[E <: Evaluator2[M] with Singleton: Witness.Aux](poly: EvaluatedPoly2[E, M])(implicit action: Action[EvaluatedMono2[E, M], G]): OrderedSet[EvaluatedMono2[E, M]] = {
      implicit def phasedEvaluatedMono: Phased[EvaluatedMono2[E, M]] = valueOf[E].evaluatedMonoPhased
      val monomials: Set[EvaluatedMono2[E, M]] = for {
        (g: G) <- grp.iterator.toSet
        i <- 0 until poly.nTerms
      } yield action.actr(poly.monomial(i), g).phaseCanonical
      val array = monomials.toArray
      spire.math.Sorting.quickSort(array)
      new OrderedSet(array.map(_.asInstanceOf[AnyRef]))
    }

    def leavesInvariant[E <: Evaluator2[M] with Singleton: Witness.Aux](poly: EvaluatedPoly2[E, M])(implicit action: Action[EvaluatedMono2[E, M], G]): Grp[G] = {
      val monomials = allElementsOf[E](poly)(implicitly, action)
      val order = M.cyclotomicOrder
      val monomialsAction = new PermutationAction[G] {
        def isFaithful: Boolean = false
        def findMovedPoint(g: G): NNOption = {
          cforRange(0 until monomials.length * order) { i =>
            if (actr(i, g) != i) return NNSome(i)
          }
          NNNone
        }
        def movedPointsUpperBound(g: G): NNOption = NNSome(monomials.length * order)
        def actr(p: Int, g: G): Int = {
          val phase = Phase(p % order, order)
          val index = p / order
          val res = monomials(index) <|+| g
          val canonical = res.phaseCanonical
          val newPhase = res.phaseOffset * phase
          val newIndex = monomials.indexOf(canonical)
          newIndex * order + newPhase.numeratorIn(order)
        }
        def actl(g: G, p: Int): Int = actr(p, g.inverse)
      }
      val nMonomials = monomials.length
      val coeffSeq = for {
        mono <- monomials.iterator.toVector
        coeff = poly.normalForm.coeff(mono.normalForm: M#Monomial)
        k <- 0 until order
        phase = Phase(k, order)
      } yield coeff * phase.toCyclo
      val partition = Partition.fromSeq(coeffSeq)
      val stabilizer = grp.orderedPartitionStabilizer(monomialsAction, partition)
      val generators = stabilizer.smallGeneratingSet
      Grp.fromGeneratorsAndOrder(generators, stabilizer.order)
    }

  }

}
