package net.alasc.symdpoly

import shapeless.Witness

import net.alasc.finite.Grp
import net.alasc.symdpoly.evaluation.Evaluator
import net.alasc.perms.default._

/** Polynomial optimization problem. */
case class Optimization[
  E <: Evaluator.Aux[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton: Witness.Aux
](direction: Direction,
  objective: E#EvaluatedPolynomial,
  operatorConstraints: Seq[OperatorConstraint[M]] = Seq.empty,
  scalarConstraints: Seq[ScalarConstraint[E, M]] = Seq.empty) {

  /** Forces the symmetrization of this optimization problem under the given group.
    *
    * No consistency checks are performed, use with caution.
    */
  def forceSymmetrizeNC(grp: Grp[M#Permutation]): Optimization[_ <: Evaluator.Aux[M] with Singleton, M] = {
    val unionGrp = E.symmetryGroup union grp
    val E1 = M.symmetricEvaluator(unionGrp, E.equivalence)
    val objective1 = E1(objective.normalForm)
    val scalarConstraints1 = scalarConstraints.map {
      case ScalarConstraint(lhs, op, rhs) => ScalarConstraint(E1(lhs.normalForm), op, E1(rhs.normalForm))
    }
    Optimization[E1.type, M](direction, objective1, operatorConstraints, scalarConstraints1)
  }

  /** Symmetrizes the optimization problem, possibly using given precomputed groups
    *
    * @param quotientFeasibilityGroup Group of permutations that preserve the quotient monoid structure,
    *                                 i.e. we have [f1] = [f2] if and only if [f1 <|+| g] = [f2 <|+| g]
    *                                 where f1, f2 are monomials in the free monoid, and g is a permutation
    * @param evaluationFeasibilityGroup Group of permutations that preserve the quotient monoid structure
    *                                   and the evaluation structure, i.e. we have
    *                                   L([f1]) = L([f2]) if and only if L([f1 <|+| g]) = L([f2 <|+| g])
    *                                   where f1, f2 are monomials in the free monoid, and g is a permutation
    * @return the symmetrized optimization problem
    */
  def symmetrize(quotientFeasibilityGroup: Option[Grp[M#Permutation]] = None,
                 evaluationFeasibilityGroup: Option[Grp[M#Permutation]] = None): Optimization[_ <: Evaluator.Aux[M] with Singleton, M] = // TODO: support constraints
    if (operatorConstraints.isEmpty && scalarConstraints.isEmpty) {
      val feasGrp = (quotientFeasibilityGroup, evaluationFeasibilityGroup) match {
        case (_, Some(efg)) => efg
        case (Some(qfg), None) => E.compatibleSubgroup(qfg)
        case (None, None) => E.compatibleSubgroup(M.symmetryGroup)
      }
      val optGrp = objective.invariantSubgroupOf(feasGrp)
      forceSymmetrizeNC(optGrp)
    } else this

  def E: E = valueOf[E]
  def M: M = valueOf[M]

  /** Constructs a moment-based/SOS relaxation. */
  def relaxation(generatingSet: GSet[M]): Relaxation[E, M] = Relaxation(this, generatingSet)

  def subjectTo(newConstraints: Constraint[E, M]*): Optimization[E, M] = {
    val newOperatorConstraints = newConstraints.collect {
      case c: OperatorConstraint[M] => c
    }
    val newScalarConstraints = newConstraints.collect {
      case c: ScalarConstraint[E, M] => c
    }
    Optimization(direction, objective, operatorConstraints ++ newOperatorConstraints, scalarConstraints ++ newScalarConstraints)
  }

}
