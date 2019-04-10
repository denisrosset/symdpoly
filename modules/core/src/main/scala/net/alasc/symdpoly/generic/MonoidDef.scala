package net.alasc.symdpoly
package generic

import scala.reflect.ClassTag

import cyclo.Cyclo

import net.alasc.symdpoly.algebra.{MultiplicativeBinoid, Phased}
import shapeless.Witness
import spire.algebra._
import spire.math.Rational
import spire.syntax.cfor.cforRange

import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.symdpoly.evaluation.{EigenvalueEvaluator, Evaluator}
import net.alasc.symdpoly.free
import net.alasc.symdpoly.math.{GenPerm, Phase}
import net.alasc.perms.default._

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
  type MonoType <: generic.Mono[self.type] // monoid element

  /** Zero monomial, which is an absorbing element of this monoid. */
  def zero: MonoType
  /** Identity element of this monoid. */
  def one: MonoType

  /** Multiplicative binoid typeclass. */
  def monoMultiplicativeBinoid: MultiplicativeBinoid[MonoType]

  /** Involution typeclass describing the Hermitian adjoint operation. */
  def monoInvolution: Involution[MonoType]

  /** Typeclass describing the total order for monomials. */
  def monoOrder: Order[MonoType]

  /** Typeclass describing the action of a phase on monomials. */
  def monoPhased: Phased[MonoType]

  /** Classtag for monomials. */
  def monoClassTag: ClassTag[MonoType]

  // Polynomials

  /** Element of the polynomial ring constructed as a linear space on this monoid. */
  type PolyType <: Poly[self.type]

  /** Associative algebra structure on the polynomials. */
  def polyAssociativeAlgebra: FieldAssociativeAlgebra[PolyType, Cyclo]

  /** Involution typeclass describing the Hermitian adjoint operation on polynomials. */
  def polyInvolution: Involution[PolyType]

  /** Polynomial equality. */
  def polyEq: Eq[PolyType]

  /** Classtag for polynomials. */
  def polyClassTag: ClassTag[PolyType]

  /** Construct a constant polynomial from the given constant. */
  def constant(i: Int): PolyType

  /** Construct a constant polynomial from the given constant. */
  def constant(r: Rational): PolyType

  /** Construct a constant polynomial from the given constant. */
  def constant(c: Cyclo): PolyType

  /** Converts a monomial to single term polynomial. */
  def monomialToPolynomial(m: MonoType): PolyType

  // Permutations

  /** Permutation acting on the monomials/monoid elements. */
  type PermutationType <: generic.Permutation[self.type]

  /** Symmetry group of this monoid. */
  def symmetryGroup: Grp[PermutationType]

  /** Permutations form a group. */
  def permutationGroup: Group[PermutationType]

  /** Permutation equality. */
  def permutationEq: Eq[PermutationType]

  /** Faithful permutation action builder. */
  def permutationFaithfulPermutationActionBuilder: FaithfulPermutationActionBuilder[PermutationType]

  /** Action of permutations on the monomials. */
  def permutationMonoAction: Action[MonoType, PermutationType]

  /** Action of permutations on polynomials. */
  def permutationPolyAction: Action[PolyType, PermutationType]

  /** Class tag for permutations. */
  def permutationClassTag: ClassTag[PermutationType]

  // Evaluator creation

  /** Creates an evaluator for eigenvalue optimization.
    *
    * @param real Whether the problem is real, i.e. we have L(x) = L(x.adjoint), where L is this evaluator
    *             By default, we do not enforce the problem to be real..
    * @param symmetryGroup Symmetry group defining additional equivalences for this evaluator.
    *                      By default, we use the trivial group (i.e. no additional equivalences).
    */
  def eigenvalueEvaluator(real: Boolean = false,
                          symmetryGroup: Grp[PermutationType] = Grp.trivial[PermutationType]
                         ): Evaluator.Aux[this.type] =
    new EigenvalueEvaluator[this.type](real, symmetryGroup)

}

object MonoidDef {

  implicit def monoMultiplicativeBinoid[M <: generic.MonoidDef with Singleton:Witness.Aux]: MultiplicativeBinoid[M#MonoType] = valueOf[M].monoMultiplicativeBinoid
  implicit def monoInvolution[M <: generic.MonoidDef with Singleton:Witness.Aux]: Involution[M#MonoType] = valueOf[M].monoInvolution
  implicit def monoOrder[M <: generic.MonoidDef with Singleton:Witness.Aux]: Order[M#MonoType] = valueOf[M].monoOrder
  implicit def monoPhased[M <: generic.MonoidDef with Singleton:Witness.Aux]: Phased[M#MonoType] = valueOf[M].monoPhased
  implicit def monoClassTag[M <: generic.MonoidDef with Singleton:Witness.Aux]: ClassTag[M#MonoType] = valueOf[M].monoClassTag

  implicit def polyAssociativeAlgebra[M <: generic.MonoidDef with Singleton:Witness.Aux]: FieldAssociativeAlgebra[M#PolyType, Cyclo] = valueOf[M].polyAssociativeAlgebra
  implicit def polyInvolution[M <: generic.MonoidDef with Singleton:Witness.Aux]: Involution[M#PolyType] = valueOf[M].polyInvolution
  implicit def polyEq[M <: generic.MonoidDef with Singleton:Witness.Aux]: Eq[M#PolyType] = valueOf[M].polyEq
  implicit def polyClassTag[M <: generic.MonoidDef with Singleton:Witness.Aux]: ClassTag[M#PolyType] = valueOf[M].polyClassTag

  implicit def permutationGroup[M <: generic.MonoidDef with Singleton:Witness.Aux]: Group[M#PermutationType] = valueOf[M].permutationGroup
  implicit def permutationEq[M <: generic.MonoidDef with Singleton:Witness.Aux]: Eq[M#PermutationType] = valueOf[M].permutationEq
  implicit def permutationFaithfulPermutationActionBuilder[M <: generic.MonoidDef with Singleton:Witness.Aux]: FaithfulPermutationActionBuilder[M#PermutationType] = valueOf[M].permutationFaithfulPermutationActionBuilder
  implicit def permutationMonoAction[M <: generic.MonoidDef with Singleton:Witness.Aux]: Action[M#MonoType, M#PermutationType] = valueOf[M].permutationMonoAction
  implicit def permutationPolyAction[M <: generic.MonoidDef with Singleton:Witness.Aux]: Action[M#PolyType, M#PermutationType] = valueOf[M].permutationPolyAction
  implicit def permutationClassTag[M <: generic.MonoidDef with Singleton:Witness.Aux]: ClassTag[M#PermutationType] = valueOf[M].permutationClassTag

}
