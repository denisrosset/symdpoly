package net.alasc.symdpoly
package freebased

import scala.collection.immutable.BitSet

import cats.Contravariant
import shapeless.Witness

import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.symdpoly.Evaluation.OpPredicate
import net.alasc.symdpoly.math.Phase
import net.alasc.symdpoly.util.OrderedSet
import net.alasc.perms.default._
import instances.invariant._

abstract class InPlaceEquivalence[
  M <: freebased.MonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
] extends Equivalence[M, F] {

  /** Applies the transformation and returns an integer such that the transformation iterated n times
    * is the identity. The returned n may not necessarily be the smallest integer having that property.
    *
    * If n == 1, then the transformation is the identity and the given monomial has not been changed.
    */
  def inPlace(mono: free.MutableWord[F]): Int

  def apply(mono0: Mono[M, F]): Set[Mono[M, F]] = {
    val mut = mono0.data.mutableCopy
    inPlace(mut) match {
      case 1 => Set(mono0)
      case n =>
        val mono1 = new Mono[M, F](mut.setImmutable())
        if (n == 2) Set(mono0, mono1) else
          Set(mono0) ++ Range(2, n).scanLeft(mono1) { case (prev, i) =>
            val current = prev.data.mutableCopy
            inPlace(current)
            new Mono[M, F](current.setImmutable())
          }
    }
  }

}

abstract class PredicateEquivalence[
  M <: freebased.MonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
] extends InPlaceEquivalence[M, F]  {

  def predicate: OpPredicate[F]

  val predicateIndex: BitSet = BitSet.empty ++ (0 until F.nOperators).filter(i => predicate(F.opFromIndex(i)))

  def groupInEvaluator(grp: Grp[M#Permutation]): Grp[M#Permutation] = {
    val set = Set(0 until F.nOperators: _*).filter(i => predicate(F.opIndexMap.elements(i)))
    val action: PermutationAction[M#Permutation] = Contravariant[PermutationAction].contramap(Perm.algebra)(_.genPerm.perm)
    grp.setwiseStabilizer(action, set)
  }

}
