package net.alasc.symdpoly
package freebased

import scala.reflect.ClassTag

import shapeless.Witness
import spire.algebra.{Action, Eq, Group}
import spire.syntax.group._

import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.symdpoly.generic.Permutation.GrpPermutationsOps
import net.alasc.symdpoly.math.{GenPerm, Phase, PhasedInt}

/** Permutation of the operator variables compatible with the structure of a monoid structure M. */
class Permutation[
  M <: MonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
](val genPerm: GenPerm) extends generic.Permutation[M] {

  def M: M = valueOf[M]
  def F: F = (M: M).Free

  override def toString: String = Permutation.prettyPrintGenPerm(genPerm, F)
  override def hashCode: Int = genPerm.hashCode
  override def equals(any: Any): Boolean = any match {
    case that: Permutation[M, F] => (this.M eq that.M) && (this.genPerm == that.genPerm)
    case _ => false
  }

}

object Permutation {

  def prettyPrintGenPerm(opAction: GenPerm, F: free.MonoidDef): String = {
    val elements = for {
      i <- 0 until F.nOperators
      PhasedInt(phase, image) = opAction.image(PhasedInt(Phase.one, i)) if phase != Phase.one || i != image
      op = F.opFromIndex(i)
      opImage = F.PhasedOp(phase, F.opFromIndex(image))
    } yield s"$op -> $opImage"
    elements.mkString("{", ", ", "}")
  }

  //region Typeclasses

  implicit def equ[
    M <: MonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ]: Eq[Permutation[M, F]] = (valueOf[M]: M).permutationEq

  implicit def group[
    M <: MonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ]: Group[Permutation[M, F]] = (valueOf[M]: M).permutationGroup

  implicit def faithfulPermutationActionBuilder[
    M <: MonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ]: FaithfulPermutationActionBuilder[Permutation[M, F]] = (valueOf[M]: M).permutationFaithfulPermutationActionBuilder

  implicit def monoAction[
    M <: MonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ]: Action[Mono[M, F], Permutation[M, F]] = (valueOf[M]: M).permutationMonoAction

  implicit def grpGenericPermutationOps[
    M <: MonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](grp: Grp[Permutation[M, F]])(implicit classTag: ClassTag[Permutation[M, F]],
                                 equ: Eq[Permutation[M, F]],
                                 fpab: FaithfulPermutationActionBuilder[Permutation[M, F]],
                                 group: Group[Permutation[M, F]]): GrpPermutationsOps[M, Permutation[M, F]] =
    new GrpPermutationsOps[M, Permutation[M, F]](grp)

  //endregion

}
