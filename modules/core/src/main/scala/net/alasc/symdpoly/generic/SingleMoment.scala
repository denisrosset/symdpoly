package net.alasc.symdpoly
package generic

import shapeless.Witness
import spire.algebra.{Action, Order}

import cyclo.Cyclo

import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.evaluation.Evaluator
import net.alasc.symdpoly.sdp.BasisTerm
import syntax.phased._

/** Evaluated monomial, which represents an equivalence class under an evaluator. */
final class SingleMoment[
  E <: Evaluator.Aux[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux
](val normalForm: M#MonoType) extends LinearMomentLike[E, M] { lhs: E#SingleMomentType =>

  def E: E = valueOf[E]
  def M: M = valueOf[M]

  def toPoly: LinearMoment[E, M] = E.apply(M.monomialToPolynomial(normalForm))

  override def toString: String = "L(" + normalForm.toString + ")"
  override def hashCode: Int = normalForm.hashCode()
  override def equals(any: Any): Boolean = any match {
    case rhs: SingleMoment[E, M] if lhs.E eq rhs.E => E.evaluatedMonoOrder.eqv(lhs, rhs)
    case _ => false
  }
  def isZero: Boolean = M.monoMultiplicativeBinoid.isZero(normalForm)(M.monoOrder)

  /** Expands this monomial on the given semidefinite relaxation. */
  def expandIn(relaxation: Relaxation[E, M], factor: Cyclo = Cyclo.one): Seq[BasisTerm] = if (isZero) Seq.empty else {
    import relaxation._
    val canonical = lhs.phaseCanonical
    val coeff = if (factor.isOne) phaseValue(lhs.phaseOffset) else cycloValue(factor * lhs.phaseOffset.toCyclo)
    val index = allMoments.indexOf(canonical)
    val indexAdjoint = adjointMoment(index)
    if (index == indexAdjoint) // monomial is self adjoint
      Seq(BasisTerm(index, coeff.real, coeff.imag))
    else if (index < indexAdjoint) // monomial value is y(index) + i * y(indexAdjoint)
      Seq(
        BasisTerm(index, coeff.real, coeff.imag),
        BasisTerm(indexAdjoint, -coeff.imag, coeff.real) // (*) see below
      )
    else // monomial value is y(indexAdjoint) - i * y(index)
      Seq(
        BasisTerm(indexAdjoint, coeff.real, coeff.imag),
        BasisTerm(index, coeff.imag, -coeff.real) // opposite of the dual term in (*)
      )
  }

}

object SingleMoment {

  //region Typeclasses

  implicit def order[
    E <: Evaluator.Aux[M] with Singleton:Witness.Aux,
    M <: generic.MonoidDef with Singleton
  ]: Order[SingleMoment[E, M]] = valueOf[E].evaluatedMonoOrder

  implicit def phased[
    E <: Evaluator.Aux[M] with Singleton:Witness.Aux,
    M <: generic.MonoidDef with Singleton
  ]: Phased[SingleMoment[E, M]] = valueOf[E].evaluatedMonoPhased

  implicit def evaluatedMonoAction[
    E <: Evaluator.Aux[M] with Singleton: Witness.Aux,
    M <: generic.MonoidDef with Singleton
  ]: Action[SingleMoment[E, M], M#PermutationType] = (valueOf[E]: E).evaluatedMonoPermutationAction

  //endregion

}
