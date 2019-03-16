package net.alasc.symdpoly

import shapeless.Witness

package object evaluation {

  /** Equivalence under the adjoint operation. */
  def real[M <: generic.MonoidDef with Singleton : Witness.Aux]: Equivalence[M] = new AdjointEquivalence[M]

  /*
  /** Equivalence under a group action. */
  def symmetric[
  M <: generic.MonoidDef with Singleton : Witness.Aux,
  G: ClassTag : FaithfulPermutationActionBuilder
  ](grp: Grp[G])(implicit action: Action[M#Monomial, G]): Equivalence[M] = SymmetryEquivalence[M, G](grp)

  /** Equivalence under cyclic permutation of operators selected by the given predicate. */
  def cyclic[
  M <: freebased.MonoidDef.Aux[F] with Singleton : Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
  ](predicate: OpPredicate[F]): Equivalence[M] = new CyclicEquivalence[M, F](predicate)

  /** Equivalence under transposition of the operators selected by the given predicate. */
  def transpose[
  M <: freebased.MonoidDef.Aux[F] with Singleton : Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
  ](predicate: OpPredicate[F]): Equivalence[M] = new TransposeEquivalence[M, F](predicate)

  /** Boolean function on operator variables. */
  trait OpPredicate[F <: free.MonoidDef with Singleton] {
    def apply(op: F#Op): Boolean
  }
  */

}