package net.alasc.symdpoly

import shapeless.Witness

import net.alasc.finite.Grp
import net.alasc.symdpoly.evaluation.{EvaluatedPoly, Symmetries}
import net.alasc.symdpoly.math.GenPerm

case class Maximization[
  E <: evaluation.FreeBasedEvaluator[M, F] with Singleton:Witness.Aux,
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux
](evaluatedPoly: EvaluatedPoly[E, M, M#TrivialGroup]) {

  def relaxation(generatingSet: GSet[M]): Relaxation[E, M, F] =
    Relaxation(this, generatingSet, valueOf[M].trivialGroup)

  /** Constructs a symmetric relaxation from the subgroup of the ambient group that preserves
    * the objective (thus computes that subgroup).
    *
    * Note that the ambient group should be compatible with both the quotient monoid
    * and the evaluation function.
    *
    */
  def symmetricRelaxation(generatingSet: GSet[M], ambientGroup: Grp[GenPerm]): Relaxation[E, M, F] =
    Relaxation(this, generatingSet, Symmetries.symmetrySubgroup(evaluatedPoly, ambientGroup))

  /** Constructs a symmetric relaxation forcing symmetry by the given group. */
  def forcedSymmetricRelaxation(generatingSet: GSet[M], group: Grp[GenPerm]): Relaxation[E, M, F] =
    Relaxation(this, generatingSet, group)

}
