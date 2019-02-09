package net.alasc.symdpoly
package generic

import cyclo.Cyclo

import net.alasc.symdpoly.algebra.{MultiplicativeBinoid, Phased}
import shapeless.Witness
import spire.algebra._

import net.alasc.finite.Grp
import net.alasc.symdpoly.evaluation.{Evaluator2, GenericEvaluator2}
import net.alasc.symdpoly.math.GenPerm

abstract class MonoidDef { self =>

  /** Maximal order of cyclotomic appearing in all constructions based on this monoid. */
  def cyclotomicOrder: Int

  // Dependent type machinery

  val witness: Witness.Aux[self.type] = Witness.mkWitness[self.type](self)
  protected implicit def impWitness: Witness.Aux[self.type] = witness

  // Trivial symmetry group

  type TrivialGroup = trivialGroupInstance.type
  val trivialGroupInstance: Grp[GenPerm] = Grp.trivial[GenPerm]
  def trivialGroup: TrivialGroup = trivialGroupInstance
  implicit val trivialGroupWitness: Witness.Aux[TrivialGroup] = Witness.mkWitness[TrivialGroup](trivialGroup)

  // Monomials

  type Monomial // monoid element

  def zero: Monomial
  def one: Monomial
  def monoMultiplicativeBinoid: MultiplicativeBinoid[Monomial]
  def monoInvolution: Involution[Monomial]
  def monoOrder: Order[Monomial]
  def monoPhased: Phased[Monomial]

  // Polynomials

  type Polynomial <: GenPoly[self.type]

  def polyAssociativeAlgebra: FieldAssociativeAlgebra[Polynomial, Cyclo]
  def polyInvolution: Involution[Polynomial]
  def polyEq: Eq[Polynomial]

  def monomialToPolynomial(m: Monomial): Polynomial

  // Construct evaluator

  def evaluator: Evaluator2[self.type] = new GenericEvaluator2[self.type](Vector.empty)(witness)

}
