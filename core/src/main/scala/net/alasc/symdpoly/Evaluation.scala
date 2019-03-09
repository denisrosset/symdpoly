package net.alasc.symdpoly

import shapeless.Witness
import spire.algebra.Action

import net.alasc.finite.Grp
import net.alasc.symdpoly.freebased.{CyclicEquivalence, MonoidDef, AdjointEquivalence, TransposeEquivalence}
import net.alasc.symdpoly.generic.{AdjointEquivalence, Equivalence, SymmetryEquivalence}

object Evaluation {

  /** Equivalence under the adjoint operation. */
  def real[M <: generic.MonoidDef with Singleton:Witness.Aux]: Equivalence[M] = new generic.AdjointEquivalence[M]

  /** Equivalence under a group action. */
  def symmetric[
    M <: generic.MonoidDef with Singleton:Witness.Aux, G
  ](grp: Grp[G])(implicit action: Action[M#Monomial, G]): Equivalence[M] = new SymmetryEquivalence[M, G](grp)

  /** Equivalence under cyclic permutation of operators selected by the given predicate. */
  def cyclic[
    M <: freebased.MonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](predicate: OpPredicate[F]): Equivalence[M] = new CyclicEquivalence[M, F](predicate)

  /** Equivalence under transposition of the operators selected by the given predicate. */
  def transpose[
    M <: freebased.MonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](predicate: OpPredicate[F]): Equivalence[M] = new TransposeEquivalence[M, F](predicate)

  /** Boolean function on operator variables. */
  trait OpPredicate[F <: free.MonoidDef with Singleton] {
    def apply(op: F#Op): Boolean
  }

}
