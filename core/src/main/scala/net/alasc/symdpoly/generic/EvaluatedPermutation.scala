package net.alasc.symdpoly
package generic

import shapeless.Witness

final class EvaluatedPermutation[
  E <: generic.Evaluator[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton
](val permutation: M#Permutation) {

  def E: E = valueOf[E]

  override def toString: String = permutation.toString

  override def hashCode: Int = permutation.hashCode

  override def equals(any: Any): Boolean = any match {
    case that: EvaluatedPermutation[E, M] => (this.E eq that.E) && (this.permutation == that.permutation)
    case _ => false
  }

}
