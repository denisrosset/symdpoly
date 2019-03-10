package net.alasc.symdpoly
package freebased

import shapeless.Witness

import net.alasc.symdpoly.evaluation.Equivalence

object Evaluator {

  def apply[
    M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](equivalences: Seq[Equivalence[M]]): evaluation.Evaluator[M] = evaluation.GenericEvaluator(equivalences)

}
