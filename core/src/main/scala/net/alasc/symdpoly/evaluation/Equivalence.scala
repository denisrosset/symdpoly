package net.alasc.symdpoly
package evaluation

import scala.annotation.tailrec
import net.alasc.finite.{Grp, GrpGroup}
import shapeless.Witness
import spire.algebra.Action
import spire.syntax.action._
import spire.syntax.cfor._
import spire.syntax.involution._
import net.alasc.symdpoly.generic.{FreeBasedMono, FreeBasedPermutation}
import net.alasc.symdpoly.math.GrpDecomposition

import scala.collection.immutable.BitSet

/** An equivalence relation on monomials. */
abstract class Equivalence[M <: generic.MonoidDef with Singleton] {

  /** Returns the set of monomials equivalent to the given monomial. */
  def apply(mono: M#Monomial): Set[M#Monomial]
}

/** Equivalence under a group action. */
final class SymmetryEquivalence[M <: generic.MonoidDef with Singleton, G](val grp: Grp[G])(implicit val action: Action[M#Monomial, G]) extends Equivalence[M] {
  def apply(mono: M#Monomial): Set[M#Monomial] = grp.iterator.map(g => mono <|+| g).toSet
}

/** Equivalence under the adjoint operation. */
final class AdjointEquivalence[M <: generic.MonoidDef with Singleton:Witness.Aux] extends Equivalence[M] {
  def M: M = valueOf[M]
  def apply(mono: M#Monomial): Set[M#Monomial] = Set(mono, M.monoInvolution.adjoint(mono))
}

/** An equivalence relation on monomials from a quotient monoid over a free monoid. */
abstract class FreeBasedEquivalence[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
] extends Equivalence[M] {

  def M: M = valueOf[M]
  def F: F = (M.Free: F)

  /** Applies the transformation and returns an integer such that the transformation iterated n times
    * is the identity. The returned n may not necessarily be the smallest integer having that property.
    *
    * If n == 1, then the transformation is the identity and the given monomial has not been changed.
    */
  def inPlace(mono: free.MutableWord[F]): Int

  def apply(mono0: FreeBasedMono[M, F]): Set[FreeBasedMono[M, F]] = {
    val mut = mono0.data.mutableCopy
    inPlace(mut) match {
      case 1 => Set(mono0)
      case n =>
        val mono1 = new FreeBasedMono[M, F](mut.setImmutable())
        if (n == 2) Set(mono0, mono1) else
          Set(mono0) ++ Range(2, n).scanLeft(mono1) { case (prev, i) =>
            val current = prev.data.mutableCopy
            inPlace(current)
            new FreeBasedMono[M, F](current.setImmutable())
          }
    }
  }

}

abstract class PredicateEquivalence[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
] extends FreeBasedEquivalence[M, F] {

  def predicate: OpPredicate[F]

  val predicateIndex: BitSet = BitSet.empty ++ (0 until F.nOperators).filter(i => predicate(F.opFromIndex(i)))

}

final class CyclicEquivalence[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
](val predicate: OpPredicate[F]) extends PredicateEquivalence[M, F] {

  /** Performs the in place cyclic permutation of the group of operators selected by the given predicate.
    * Elements are shifted one place to the left.
    */
  def inPlace(mono: free.MutableWord[F]): Int = {
    val n = mono.length

    @tailrec def findNext(i: Int): Int =
      if (i == n) n
      else if (predicateIndex(mono.indices(i))) i
      else findNext(i + 1)

    val first = findNext(0)
    if (first == n) return 1
    val second = findNext(first)
    if (second == n) return 1

    @tailrec def iterate(prev: Int, current: Int, count: Int): Int =
      if (current == n) {
        count
      } else {
        mono.swap(prev, current)
        iterate(current, findNext(current + 1), count + 1)
      }

    iterate(first, second, 0)
  }

}

final class TransposeEquivalence[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
](val predicate: OpPredicate[F]) extends PredicateEquivalence[M, F] {

  /** Performs the in place partial transpose of the group of operators selected by the given predicate.
    *
    * Returns whether any changes have been made.
    */
  def inPlace(mono: free.MutableWord[F]): Int =
    if (mono.isZero || mono.isOne || mono.isMinusOne) 1 else {
      @tailrec def iter(l: Int, r: Int, changed: Boolean): Boolean =
        if (l > r) changed
        else if (l == r) {
          if (predicateIndex(mono.indices(l))) {
            val la = F.indexAdjoint(mono.indices(l))
            if (la != mono.indices(l)) {
              mono.indices(l)
              true
            } else changed
          } else changed
        } else if (predicateIndex(mono.indices(l)) && predicateIndex(mono.indices(r))) {
          val la = F.indexAdjoint(mono.indices(l))
          if (la != mono.indices(r)) {
            val ra = F.indexAdjoint(mono.indices(r))
            mono.indices(l) = ra
            mono.indices(r) = la
            iter(l + 1, r - 1, true)
          }
          else iter(l + 1, r - 1, changed)
        } else if (predicateIndex(mono.indices(l))) iter(l, r - 1, changed)
        else if (predicateIndex(mono.indices(r))) iter(l + 1, r, changed)
        else iter(l + 1, r - 1, changed)

      if (iter(0, mono.length - 1, false)) 2 else 1
    }

}

final class FullAdjointEquivalence[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
] extends FreeBasedEquivalence[M, F] {

  /** Performs the in place transpose of the given monomial.
    *
    * Returns whether any changes have been made.
    */
  def inPlace(mono: free.MutableWord[F]): Int =
    if (mono.isZero || mono.isOne || mono.isMinusOne) 1 else {
      @tailrec def iter(l: Int, r: Int, changed: Boolean): Boolean =
        if (l > r) changed
        else {
          val la = F.indexAdjoint(mono.indices(l))
          if (la != mono.indices(r)) {
            val ra = F.indexAdjoint(mono.indices(r))
            mono.indices(l) = ra
            mono.indices(r) = la
            iter(l + 1, r - 1, true)
          }
          else iter(l + 1, r - 1, changed)
        }

      if (iter(0, mono.length - 1, false)) 2 else 1
    }

}
