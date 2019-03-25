package net.alasc.symdpoly
package generic

import scala.reflect.ClassTag

import cyclo.Cyclo

import net.alasc.symdpoly.algebra.{MultiplicativeBinoid, Phased}
import shapeless.Witness
import spire.algebra._
import spire.math.Rational

import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.symdpoly.evaluation.{Equivalence, Evaluator}

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

  /** Classtag for monomials. */
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

  /** Classtag for polynomials. */
  def polyClassTag: ClassTag[Polynomial]

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

  /** Symmetry group of this monoid. */
  def symmetryGroup: Grp[Permutation]

  /** Permutations form a group. */
  def permutationGroup: Group[Permutation]

  /** Permutation equality. */
  def permutationEq: Eq[Permutation]

  /** Faithful permutation action builder. */
  def permutationFaithfulPermutationActionBuilder: FaithfulPermutationActionBuilder[Permutation]

  /** Action of permutations on the monomials. */
  def permutationMonoAction: Action[Monomial, Permutation]

  /** Class tag for permutations. */
  def permutationClassTag: ClassTag[Permutation]

  /** Constructs an evaluator over this monoid without enforced symmetries. */
  def evaluator(equivalence: Equivalence[self.type]): Evaluator.Aux[self.type] =
    symmetricEvaluator(Grp.trivial[Permutation], equivalence)

  /** Construct an evaluator over this monoid without enforced symmetries nor equivalence relation. */
  def evaluator(): Evaluator.Aux[self.type] = evaluator(net.alasc.symdpoly.evaluation.trivial[self.type])

  /** Constructs an evaluator over this monoid with the given symmetry enforced but no equivalence relation. */
  def symmetricEvaluator(symmetryGroup: Grp[Permutation]): Evaluator.Aux[self.type] =
    symmetricEvaluator(symmetryGroup, net.alasc.symdpoly.evaluation.trivial[self.type])

    /** Constructs an evaluator over this monoid with the given symmetry enforced. */
  def symmetricEvaluator(symmetryGroup0: Grp[Permutation], equivalence0: Equivalence[self.type]): Evaluator.Aux[self.type] = new Evaluator { evaluator =>
    val equivalence: Equivalence[self.type] = equivalence0
    val symmetryGroup: Grp[Permutation] = symmetryGroup0
    type Mono = self.type
    val witnessMono: Witness.Aux[self.type] = self.witness
  }

}

object MonoidDef {

  implicit def monoMultiplicativeBinoid[M <: generic.MonoidDef with Singleton:Witness.Aux]: MultiplicativeBinoid[M#Monomial] = valueOf[M].monoMultiplicativeBinoid
  implicit def monoInvolution[M <: generic.MonoidDef with Singleton:Witness.Aux]: Involution[M#Monomial] = valueOf[M].monoInvolution
  implicit def monoOrder[M <: generic.MonoidDef with Singleton:Witness.Aux]: Order[M#Monomial] = valueOf[M].monoOrder
  implicit def monoPhased[M <: generic.MonoidDef with Singleton:Witness.Aux]: Phased[M#Monomial] = valueOf[M].monoPhased
  implicit def monoClassTag[M <: generic.MonoidDef with Singleton:Witness.Aux]: ClassTag[M#Monomial] = valueOf[M].monoClassTag

  implicit def polyAssociativeAlgebra[M <: generic.MonoidDef with Singleton:Witness.Aux]: FieldAssociativeAlgebra[M#Polynomial, Cyclo] = valueOf[M].polyAssociativeAlgebra
  implicit def polyInvolution[M <: generic.MonoidDef with Singleton:Witness.Aux]: Involution[M#Polynomial] = valueOf[M].polyInvolution
  implicit def polyEq[M <: generic.MonoidDef with Singleton:Witness.Aux]: Eq[M#Polynomial] = valueOf[M].polyEq
  implicit def polyClassTag[M <: generic.MonoidDef with Singleton:Witness.Aux]: ClassTag[M#Polynomial] = valueOf[M].polyClassTag

  implicit def permutationGroup[M <: generic.MonoidDef with Singleton:Witness.Aux]: Group[M#Permutation] = valueOf[M].permutationGroup
  implicit def permutationEq[M <: generic.MonoidDef with Singleton:Witness.Aux]: Eq[M#Permutation] = valueOf[M].permutationEq
  implicit def permutationFaithfulPermutationActionBuilder[M <: generic.MonoidDef with Singleton:Witness.Aux]: FaithfulPermutationActionBuilder[M#Permutation] = valueOf[M].permutationFaithfulPermutationActionBuilder
  implicit def permutationMonoAction[M <: generic.MonoidDef with Singleton:Witness.Aux]: Action[M#Monomial, M#Permutation] = valueOf[M].permutationMonoAction
  implicit def permutationClassTag[M <: generic.MonoidDef with Singleton:Witness.Aux]: ClassTag[M#Permutation] = valueOf[M].permutationClassTag

}
