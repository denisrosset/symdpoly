package net.alasc.symdpoly

import cats.Contravariant
import shapeless.Witness
import shapeless.Witness.Aux

import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp
import net.alasc.partitions.Partition
import net.alasc.symdpoly.evaluation.internal.{AdjointEquivalence, ComposedEquivalence, CyclicEquivalence, TransposeEquivalence, TransposesEquivalence}
import instances.invariant._
import net.alasc.perms.default._
import net.alasc.perms.Perm

package object evaluation {

  /** Boolean function on operator variables. */
  trait OpPredicate[F <: free.MonoidDef with Singleton] {
    def apply(op: F#Op): Boolean
  }

  trait OpGroup[F <: free.MonoidDef with Singleton] {
    def apply(op: F#Op): Option[Int]
  }

  /** Returns a trivial equivalence relation, where the equivalence class of m is Set(m). */
  def trivial[M <: generic.MonoidDef with Singleton: Witness.Aux]: Equivalence[M] = new Equivalence[M] {
    def witnessM: Aux[M] = implicitly
    def apply(mono: M#Monomial): Set[M#Monomial] = Set(mono)
    def compatibleSubgroup(grp: Grp[M#Permutation]): Grp[M#Permutation] = grp
    def isSelfAdjoint: Boolean = false
  }

  /** Equivalence under the adjoint operation. */
  def real[M <: generic.MonoidDef with Singleton : Witness.Aux]: Equivalence[M] = AdjointEquivalence[M]()

  def compose[M <: generic.MonoidDef with Singleton : Witness.Aux](equivalences: Equivalence[M]*): Equivalence[M] =
    equivalences match {
      case Seq() => trivial
      case Seq(e) => e
      case seq => ComposedEquivalence(seq)
    }

  /** Equivalence under cyclic permutation of operators selected by the given predicate. */
  def cyclic[
    M <: freebased.MonoidDef.Aux[F] with Singleton : Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](predicate: OpPredicate[F]): Equivalence[M] = CyclicEquivalence[M, F](predicate)

  /** Equivalence under transposition of the operators selected by the given predicate. */
  def transpose[
    M <: freebased.MonoidDef.Aux[F] with Singleton : Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](predicate: OpPredicate[F]): Equivalence[M] = TransposeEquivalence[M, F](predicate)

  /** Equivalence under transposition of the groups of operators specified by the given group predicate */
  def transposes[
    M <: freebased.MonoidDef.Aux[F] with Singleton : Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton : Witness.Aux
  ](groups: OpGroup[F]): Equivalence[M] = TransposesEquivalence[M, F](groups)

}
