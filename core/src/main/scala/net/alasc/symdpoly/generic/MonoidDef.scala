package net.alasc.symdpoly
package generic

import scala.reflect.ClassTag

import cyclo.Cyclo

import net.alasc.symdpoly.algebra.{MultiplicativeBinoid, Phased}
import shapeless.Witness
import spire.algebra._
import spire.math.Rational

/** Describes a generic monomial monoid. */
abstract class MonoidDef { self =>

  /** Maximal order of cyclotomic appearing in all constructions based on this monoid. */
  def cyclotomicOrder: Int

  // Dependent type machinery

  /** Witness that transports an instance of this monoid where it is needed through the implicit mechanism. */
  val witness: Witness.Aux[self.type] = Witness.mkWitness[self.type](self)
  protected implicit def impWitness: Witness.Aux[self.type] = witness

  // Monomials

  /** Element of this monoid. */
  type Monomial <: generic.Mono[self.type] // monoid element

  /** Zero monomial, which is an absorbing element of this monoid. */
  def zero: Monomial
  /** Identity element of this monoid. */
  def one: Monomial

  /** Multiplicative binoid typeclass. */
  def monoMultiplicativeBinoid: MultiplicativeBinoid[Monomial]

  /** Involution typeclass describing the Hermitian adjoint operation. */
  def monoInvolution: Involution[Monomial]

  /** Typeclass describing the total order for monomials. */
  def monoOrder: Order[Monomial]

  /** Typeclass describing the action of a phase on monomials. */
  def monoPhased: Phased[Monomial]

  def monoClassTag: ClassTag[Monomial]

  // Polynomials

  /** Element of the polynomial ring constructed as a linear space on this monoid. */
  type Polynomial <: Poly[self.type]

  /** Associative algebra structure on the polynomials. */
  def polyAssociativeAlgebra: FieldAssociativeAlgebra[Polynomial, Cyclo]

  /** Involution typeclass describing the Hermitian adjoint operation on polynomials. */
  def polyInvolution: Involution[Polynomial]

  /** Polynomial equality. */
  def polyEq: Eq[Polynomial]

  /** Construct a constant polynomial from the given constant. */
  def constant(i: Int): Polynomial

  /** Construct a constant polynomial from the given constant. */
  def constant(r: Rational): Polynomial

  /** Construct a constant polynomial from the given constant. */
  def constant(c: Cyclo): Polynomial

  /** Converts a monomial to single term polynomial. */
  def monomialToPolynomial(m: Monomial): Polynomial

  // Permutations

  /** Permutation acting on the monomials/monoid elements. */
  type Permutation <: generic.Permutation[self.type]

  /** Group of permutations. */
  def permutationGroup: Group[Permutation]

  /** Action of permutations on the monomials. */
  def permutationMonoAction: Action[Monomial, Permutation]

  /** Default evaluator without additional equivalence relations. */
  def evaluator(equivalences: Equivalence[self.type]*): Evaluator[self.type] = new generic.Evaluator[self.type](equivalences)

}
