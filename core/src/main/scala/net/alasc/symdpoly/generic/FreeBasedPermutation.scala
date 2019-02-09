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
import net.alasc.symdpoly.generic.GenericPermutation.GrpGenericPermutationOps
import net.alasc.util.{NNNone, NNOption, NNSome}

class FreeBasedPermutation[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
](val genPerm: GenPerm) extends GenericPermutation[M] {
  def M: M = valueOf[M]
  def F: F = (M: M).Free
  override def toString: String = FreeBasedPermutation.prettyPrintGenPerm(genPerm, F)
  override def hashCode: Int = genPerm.hashCode
  override def equals(any: Any): Boolean = any match {
    case that: FreeBasedPermutation[M, F] => (this.M eq that.M) && (this.genPerm == that.genPerm)
    case _ => false
  }
}

object FreeBasedPermutation {

  def prettyPrintGenPerm(opAction: GenPerm, F: free.MonoidDef): String = {
    val elements = for {
      i <- 0 until F.nOperators
      PhasedInt(phase, image) = opAction.image(PhasedInt(Phase.one, i)) if phase != Phase.one || i != image
      op = F.opFromIndex(i)
      opImage = F.PhasedOp(phase, F.opFromIndex(image))
    } yield s"$op -> $opImage"
    elements.mkString("{", ", ", "}")
  }

  implicit def evaluatedMonoAction[
    E <: Evaluator2[M] with Singleton: Witness.Aux,
    M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](implicit action: Action[M#Monomial, FreeBasedPermutation[M, F]]): Action[EvaluatedMono2[E, M], FreeBasedPermutation[M, F]] =
    Invariant[Lambda[P => Action[P, FreeBasedPermutation[M, F]]]].imap[M#Monomial, EvaluatedMono2[E, M]](action)((mono: M#Monomial) => (valueOf[E]: E).apply(mono))(_.normalForm)

  implicit def equ[
    M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ]: Eq[FreeBasedPermutation[M, F]] = Eq[GenPerm].contramap(_.genPerm)

  implicit def group[
    M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ]: Group[FreeBasedPermutation[M, F]] = Group[GenPerm].imap(new FreeBasedPermutation[M, F](_))(_.genPerm)

  implicit def faithfulPermutationActionBuilder[
    M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ]: FaithfulPermutationActionBuilder[FreeBasedPermutation[M, F]] =
    FaithfulPermutationActionBuilder[GenPerm].contramap(_.genPerm)

  implicit def monoAction[
    M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ]: Action[Mono[M, F], FreeBasedPermutation[M, F]] = new FreeBasedPermutationMonoAction[M, F]

  implicit def grpGenericPermutationOps[
    M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](grp: Grp[FreeBasedPermutation[M, F]])(implicit classTag: ClassTag[FreeBasedPermutation[M, F]],
                                     equ: Eq[FreeBasedPermutation[M, F]],
                                     fpab: FaithfulPermutationActionBuilder[FreeBasedPermutation[M, F]],
                                     group: Group[FreeBasedPermutation[M, F]]): GrpGenericPermutationOps[M, FreeBasedPermutation[M, F]] =
    new GrpGenericPermutationOps[M, FreeBasedPermutation[M, F]](grp)

}

class FreeBasedPermutationMonoAction[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
] extends Action[Mono[M, F], FreeBasedPermutation[M, F]] {
  def actl(g: FreeBasedPermutation[M, F], mono: Mono[M, F]): Mono[M, F] = actr(mono, g.inverse)
  def actr(mono: Mono[M, F], g: FreeBasedPermutation[M, F]): Mono[M, F] = {
    val word = mono.normalForm.mutableCopy.applyGenPermAction(g.genPerm)
    valueOf[M].inPlaceNormalForm(word)
    new Mono[M, F](word.setImmutable())
  }
}
