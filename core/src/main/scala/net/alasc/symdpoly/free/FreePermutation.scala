package net.alasc.symdpoly
package free

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

class FreePermutation[F <: free.MonoidDef with Singleton:Witness.Aux](val genPerm: GenPerm) extends GenericPermutation[F] {
  def F: F = valueOf[F]
  override def toString: String = FreePermutation.prettyPrintGenPerm(genPerm, valueOf[F])
  override def hashCode: Int = genPerm.hashCode
  override def equals(any: Any): Boolean = any match {
    case that: FreePermutation[F] => (this.F eq that.F) && (this.genPerm == that.genPerm)
    case _ => false
  }
}

object FreePermutation {

  def prettyPrintGenPerm(opAction: GenPerm, F: MonoidDef): String = {
    val elements = for {
      i <- 0 until F.nOperators
      PhasedInt(phase, image) = opAction.image(PhasedInt(Phase.one, i)) if phase != Phase.one || i != image
      op = F.opFromIndex(i)
      opImage = F.PhasedOp(phase, F.opFromIndex(image))
    } yield s"$op -> $opImage"
    elements.mkString("{", ", ", "}")
  }

  // TODO: move this to free.MonoidDef

  implicit def faithfulPermutationActionBuilder[F <: free.MonoidDef with Singleton]: FaithfulPermutationActionBuilder[FreePermutation[F]] =
    FaithfulPermutationActionBuilder[GenPerm].contramap(_.genPerm)

  implicit def equ[F <: free.MonoidDef with Singleton]: Eq[FreePermutation[F]] = Eq[GenPerm].contramap(_.genPerm)

  implicit def group[F <: free.MonoidDef with Singleton:Witness.Aux]: Group[FreePermutation[F]] = Invariant[Group].imap(Group[GenPerm])(new FreePermutation[F](_))(_.genPerm)

  implicit def freeMonoAction[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux]: Action[Mono[F, F], FreePermutation[F]] =  new FreeMonoOpPermutationAction[F]

}

class FreeMonoOpPermutationAction[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux] extends Action[Mono[F, F], FreePermutation[F]] {
  def actr(mono: Mono[F, F], g: FreePermutation[F]): Mono[F, F] = {
    val res = mono.mutableCopy
    res.applyGenPermAction(g.genPerm)
    new Mono[F, F](res.setImmutable())
  }

  def actl(g: FreePermutation[F], mono: Mono[F, F]): Mono[F, F] = actr(mono, g.inverse)
}

class QuotientMonoPermutationAction[
  M <: quotient.MonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
] extends Action[Mono[M, F], QuotientPermutation[M]] {
  def actl(g: QuotientPermutation[M], mono: Mono[M, F]): Mono[M, F] = actr(mono, g.inverse)
  def actr(mono: Mono[M, F], g: QuotientPermutation[M]): Mono[M, F] = {
    val word = mono.normalForm.mutableCopy.applyGenPermAction(g.genPerm)
    valueOf[M].inPlaceNormalForm(word)
    new Mono[M, F](word.setImmutable())
  }
}

class QuotientPermutation[
  M <: quotient.MonoidDef with Singleton: Witness.Aux
](val genPerm: GenPerm) extends GenericPermutation[M] {
  def M: M = valueOf[M]
  def F: free.MonoidDef = valueOf[M].Free
  override def toString: String = FreePermutation.prettyPrintGenPerm(genPerm, F)
  override def hashCode: Int = genPerm.hashCode
  override def equals(any: Any): Boolean = any match {
    case that: QuotientPermutation[M] => (this.M eq that.M) && (this.genPerm == that.genPerm)
    case _ => false
  }
}

object QuotientPermutation {

  def applyUnsafe[M <: quotient.MonoidDef with Singleton:Witness.Aux](g: FreePermutation[M#Free]): QuotientPermutation[M] =
    new QuotientPermutation[M](g.genPerm)

  implicit def faithfulPermutationActionBuilder[M <: quotient.MonoidDef with Singleton]: FaithfulPermutationActionBuilder[QuotientPermutation[M]] =
    FaithfulPermutationActionBuilder[GenPerm].contramap(_.genPerm)

  implicit def equ[M <: quotient.MonoidDef with Singleton]: Eq[QuotientPermutation[M]] = Eq[GenPerm].contramap(_.genPerm)

  implicit def group[M <: quotient.MonoidDef with Singleton:Witness.Aux]: Group[QuotientPermutation[M]] = Group[GenPerm].imap(new QuotientPermutation[M](_))(_.genPerm)

  implicit def quotientMonoAction[M <: quotient.MonoidDef with Singleton:Witness.Aux]: Action[M#Monomial, QuotientPermutation[M]] = {
    val res = new QuotientMonoPermutationAction[M with quotient.MonoidDef.Aux[M#Free] with Singleton, M#Free]()(valueOf[M].witness.asInstanceOf[Witness.Aux[M with quotient.MonoidDef.Aux[M#Free] with Singleton]])
    res.asInstanceOf[Action[M#Monomial, QuotientPermutation[M]]]
  }

  implicit def evaluatedMonoAction[
    E <: Evaluator2[M] with Singleton: Witness.Aux,
    M <: quotient.MonoidDef with Singleton
  ](implicit action: Action[M#Monomial, QuotientPermutation[M]]): Action[EvaluatedMono2[E, M], QuotientPermutation[M]] =
    Invariant[Lambda[P => Action[P, QuotientPermutation[M]]]].imap[M#Monomial, EvaluatedMono2[E, M]](action)((mono: M#Monomial) => (valueOf[E]: E).apply(mono))(_.normalForm)

  implicit def grpQuotientPermutationOps[M <: quotient.MonoidDef with Singleton: Witness.Aux](grp: Grp[QuotientPermutation[M]]): GenericPermutation.GrpGenericPermutationOps[M, QuotientPermutation[M]] =
    new GenericPermutation.GrpGenericPermutationOps[M, QuotientPermutation[M]](grp)

}
