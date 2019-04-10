package net.alasc.symdpoly
package evaluation
package components

import net.alasc.symdpoly.freebased.Mono
/*
abstract class InPlaceComponent[
  M <: freebased.MonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
] extends FreeBasedComponent[M, F] {

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
        M.inPlaceNormalForm(mut)
        val mono1 = new Mono[M, F](mut.setImmutable())
        if (n == 2) Set(mono0, mono1) else
          Set(mono0) ++ Range(2, n).scanLeft(mono1) { case (prev, i) =>
            val current = prev.data.mutableCopy
            inPlace(current)
            M.inPlaceNormalForm(current)
            new Mono[M, F](current.setImmutable())
          }
    }
  }

}
*/