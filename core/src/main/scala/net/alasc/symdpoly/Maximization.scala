package net.alasc.symdpoly

import shapeless.Witness

import net.alasc.finite.Grp
import net.alasc.symdpoly.evaluation.{EvaluatedPoly, EvaluatedPoly2, Symmetries}
import net.alasc.symdpoly.math.GenPerm
import spire.std.int._

case class Maximization[
  E <: evaluation.FreeBasedEvaluator[M, F] with Singleton,
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
](evaluatedPoly: EvaluatedPoly[E, M, M#TrivialGroup])
 (implicit wE: Witness.Aux[E], wM: Witness.Aux[M], wF: Witness.Aux[F]) {

  def E: E = wE.value
  def M: M = wM.value
  def F: F = wF.value

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

  def cyclotomicOrder(poly: GenPoly[_]): Int = (0 until poly.nTerms).map(i => poly.coeff(i).order).foldLeft(2)(spire.math.lcm[Int](_, _))

  def symmetricRelaxation(generatingSet: GSet[M]): Relaxation[E, M, F] = {
    // TODO: prove that it is enough
    val ambientGroup = M.symmetryGroup(cyclotomicOrder(evaluatedPoly.normalForm))
    symmetricRelaxation(generatingSet, ambientGroup)
  }

  /** Constructs a symmetric relaxation forcing symmetry by the given group. */
  def forcedSymmetricRelaxation(generatingSet: GSet[M], group: Grp[GenPerm]): Relaxation[E, M, F] =
    Relaxation(this, generatingSet, group)

}

