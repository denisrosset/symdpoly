package net.alasc.symdpoly

import net.alasc.finite.Grp
import net.alasc.symdpoly.generic.FreeBasedMonoidDef
import net.alasc.symdpoly.generic.FreeBasedPermutation.FreeBasedPermutationMonoAction
import shapeless.Witness
import spire.algebra.Action

package object evaluation {

  /** Equivalence under the adjoint operation. */
  def real[M <: generic.MonoidDef with Singleton:Witness.Aux]: Equivalence[M] = valueOf[M] match {
    case freeBased: FreeBasedMonoidDef.Aux[free] with Singleton => // recover the underlying type
      val equiv = new FullAdjointEquivalence[freeBased.type, free]()(freeBased.witness)
      equiv.asInstanceOf[Equivalence[M]] // escape hatch, we know that freeBased.type == M
    case _ => new AdjointEquivalence[M]
  }

  /** Equivalence under a group action. */
  def symmetric[
    M <: generic.MonoidDef with Singleton:Witness.Aux,
    G
  ](grp: Grp[G])(implicit action: Action[M#Monomial, G]): Equivalence[M] = {
    def generic: Equivalence[M] = new SymmetryEquivalence[M, G](grp)
    valueOf[M] match {
      case freeBasedMonoid: FreeBasedMonoidDef.Aux[free] => action match {
        case freeBasedAction: FreeBasedPermutationMonoAction[freeBasedMonoid.type, free] => generic
        case _ => generic
      }
      case _ => generic
    }
  }

  /** Equivalence under cyclic permutation of operators selected by the given predicate. */
  def cyclic[
    M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](predicate: OpPredicate[F]): Equivalence[M] = new CyclicEquivalence[M, F](predicate)

  /** Equivalence under transposition of the operators selected by the given predicate. */
  def transpose[
    M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](predicate: OpPredicate[F]): Equivalence[M] = new TransposeEquivalence[M, F](predicate)

}
