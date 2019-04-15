package net.alasc.symdpoly
package freebased

import shapeless.Witness
import spire.algebra.Action
import spire.syntax.group._

class PermutationMonoAction[
  M <: freebased.MonoDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoDef.Aux[F] with Singleton
] extends Action[Mono[M, F], freebased.Permutation[M, F]] {
  def actl(g: freebased.Permutation[M, F], mono: freebased.Mono[M, F]): freebased.Mono[M, F] = actr(mono, g.inverse)
  def actr(mono: freebased.Mono[M, F], g: freebased.Permutation[M, F]): freebased.Mono[M, F] = {
    val word = mono.normalForm.mutableCopy.inPlaceGenPermAction(g.genPerm)
    valueOf[M].inPlaceNormalForm(word)
    new Mono[M, F](word.setImmutable())
  }
}
