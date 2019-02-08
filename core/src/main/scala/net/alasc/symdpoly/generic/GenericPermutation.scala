package net.alasc.symdpoly
package generic

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
import spire.algebra.{Action, Eq, Group}

import cyclo.Cyclo

import net.alasc.partitions.Partition
import net.alasc.perms.Perm
import net.alasc.symdpoly
import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.algebra.Phased.syntax._
import net.alasc.symdpoly.evaluation._
import net.alasc.symdpoly.evaluation.Symmetries.{allEvaluatedMonomials, momentSetAction}
import net.alasc.symdpoly.quotient.QuotientPermutation

trait GenericPermutation[M <: generic.MonoidDef with Singleton]

object GenericPermutation {

  implicit def evaluatedMonoAction[
  E <: Evaluator2[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton
  ](implicit action: Action[M#Monomial, GenericPermutation[M]]): Action[EvaluatedMono2[E, M], GenericPermutation[M]] =
    Invariant[Lambda[P => Action[P, GenericPermutation[M]]]].imap[M#Monomial, EvaluatedMono2[E, M]](action)((mono: M#Monomial) => (valueOf[E]: E).apply(mono))(_.normalForm)

  implicit def grpGenericPermutationOps[M <: quotient.MonoidDef with Singleton: Witness.Aux](grp: Grp[GenericPermutation[M]]): GrpGenericPermutationOps[M, GenericPermutation[M]] =
    new GenericPermutation.GrpGenericPermutationOps[M, GenericPermutation[M]](grp)

  implicit class GrpGenericPermutationOps[
  M <: generic.MonoidDef with Singleton: Witness.Aux,
  G <: GenericPermutation[M]
  ](grp: Grp[G]){

    def M: M = valueOf[M]

    def allElementsOf[E <: Evaluator2[M] with Singleton: Witness.Aux](poly: EvaluatedPoly2[E, M])(implicit action: Action[EvaluatedMono2[E, M], G]) : OrderedSet[EvaluatedMono2[E, M]] = {
      implicit def phasedEvaluatedMono: Phased[EvaluatedMono2[E, M]] = valueOf[E].evaluatedMonoPhased
      val monomials: Set[EvaluatedMono2[E, M]] = for {
        (g: G) <- grp.iterator.toSet
        i <- 0 until poly.nTerms
      } yield action.actr(poly.monomial(i), g).phaseCanonical
      val array = monomials.toArray
      spire.math.Sorting.quickSort(array)
      new OrderedSet(array.map(_.asInstanceOf[AnyRef]))
    }
    /*
        def stabilizes[E <: Evaluator2[M] with Singleton: Witness.Aux](poly: EvaluatedPoly2[E, M])(implicit action: Action[EvaluatedMono2[E, M], G]): Grp[G] = {
          val monomials = allEvaluatedMonomials[E](poly)
          val rootOrder = M.cyclotomicOrder
          val nMonomials = monomials.length
          val nOperators = valueOf[F].nOperators
              val conj = GenPerm(
                Perm.fromImages(
                  Vector.range(nMonomials, nMonomials + nOperators) ++ Vector.range(0, nMonomials)
                ), Phases.empty
              )
              val action = new GenPermFaithfulPermutationAction(nMonomials + nOperators, rootOrder)
              val fullDomainGenerators = ambientGroup.generators.map { g =>
                momentSetAction[E, M, F](monomials, g) |+| conj.inverse |+| g |+| conj
              }
              val coeffSeq = (for {
                mono <- monomials.iterator.toVector
                coeff = poly.normalForm.coeff(mono.normalForm: Mono[M, F])
                k <- 0 until rootOrder
                phase = Phase(k, rootOrder)
              } yield coeff * phase.toCyclo) ++ Seq.fill(nOperators * rootOrder)(Cyclo.zero)
              val partition = Partition.fromSeq(coeffSeq)
              val fullDomainSubgroupGenerators = Grp(fullDomainGenerators: _*) // construct group with full action
                .orderedPartitionStabilizer(action, partition) // find subgroup that fixes partition
                .smallGeneratingSet // find small subset of generators
              val subgroupGenerators = fullDomainSubgroupGenerators.map(g => (conj |+| g |+| conj.inverse).truncate(nOperators))
              Grp(subgroupGenerators: _*)
            }*/
  }

}
