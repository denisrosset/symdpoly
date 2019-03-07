package net.alasc.symdpoly
package generic

import cats.Invariant
import cyclo.Cyclo
import cats.instances.invariant._
import cats.syntax.invariant._
import cats.syntax.contravariant._
import cats.instances.eq._
import net.alasc.algebra.PermutationAction
import net.alasc.finite.{FaithfulActionBuilder, FaithfulPermutationActionBuilder, Grp}
import net.alasc.partitions.Partition
import net.alasc.symdpoly.algebra.{MultiplicativeBinoid, Phased}
import net.alasc.symdpoly.free._
import net.alasc.symdpoly.math.GenPerm
import net.alasc.perms.default._
import net.alasc.util._
import shapeless.Witness
import spire.algebra.{free => _, _}
import instances.all._
import net.alasc.symdpoly.evaluation.{EvaluatedMono, Evaluator, FreeBasedEvaluator, GenericFreeBasedEvaluator}
import net.alasc.symdpoly.generic.FreeBasedMonoidDef.PolyInstances

/** Monoid whose elements are represented by normal forms in a free monoid.
  * Is not necessarily a strict quotient monoid, as free.MonoidDef inherits from
  * this; then code can be reused between free monoids and their quotients.
  */
abstract class FreeBasedMonoidDef extends generic.MonoidDef { self =>

  //region Members to implement

  /** Free monoid [[free.MonoidDef]] on which this monoid is based. */
  type Free <: free.MonoidDef with Singleton { type Free = self.Free }
  /** Free monoid instance. */
  def Free: Free
  /** Witness of the free monoid instance. */
  implicit def witnessFree: Witness.Aux[Free] = Free.witness

  def inPlaceNormalForm(word: MutableWord[Free], start: Int = 0): Boolean

  /** Quotient map from the free monoid to the quotient monoid. */
  def quotient(word: FreeBasedMono[Free, Free]): Monomial

  /** Quotient map from the polynomial ring on the free monoid to the polynomial ring on the quotient monoid. */
  def quotient(poly: FreeBasedPoly[Free, Free]): FreeBasedPoly[self.type, Free]

  //endregion

  // TODO: use a [[ScratchPad]] based evaluator
  override def evaluator: GenericFreeBasedEvaluator[self.type, Free] = new GenericFreeBasedEvaluator[self.type, Free](Vector.empty)

  //region Monomials

  /** Element of this monoid, i.e. a monomial. */
  type Monomial = FreeBasedMono[self.type, Free]

  /** Quotient map applied to generating sets of monomials. */
  def quotient(gset: GSet[Free]): GSet[self.type] = GSet.Quotient[self.type, Free](gset)


  // Monomial typeclass instances

  private[this] val monoInstances: FreeBasedMono.FreeBasedMonoInstances[self.type, Free] = new FreeBasedMono.FreeBasedMonoInstances[self.type, Free]

  def monoMultiplicativeBinoid: MultiplicativeBinoid[Monomial] = monoInstances
  def monoInvolution: Involution[Monomial] = monoInstances
  def monoOrder: Order[Monomial] = monoInstances
  val monoPhased: Phased[Monomial] = new FreeBasedMono.FreeBasedMonoPhased
  val monoGenPermAction: Action[Monomial, GenPerm] = new FreeBasedMono.FreeBasedMonoGenPermAction

  val zero: Monomial = FreeBasedMono.zero[self.type, Free]
  val one: Monomial = FreeBasedMono.one[self.type, Free]

  //endregion

  //region Polynomials

  /** Polynomial are elements of the ring defined on this monoid. */
  type Polynomial = FreeBasedPoly[self.type, Free]

  /** Returns a polynomial containing a single monomial term. */
  def monomialToPolynomial(m: FreeBasedMono[self.type, Free]): FreeBasedPoly[self.type, Free] = FreeBasedPoly[self.type, Free](m)

  // Polynomial typeclass instances

  private[this] val polyInstances: PolyInstances[self.type, Free] = new PolyInstances[self.type, Free]
  def polyAssociativeAlgebra: FieldAssociativeAlgebra[Polynomial, Cyclo] = polyInstances
  def polyInvolution: Involution[Polynomial] = polyInstances
  def polyEq: Eq[Polynomial] = polyInstances
  val polyGenPermAction: Action[FreeBasedPoly[self.type, Free], GenPerm] = new FreeBasedPoly.PolyGenPermAction

  //endregion

  //region Permutations

  type Permutation = generic.FreeBasedPermutation[self.type, Free]

  val permutationEq: Eq[Permutation] = Eq[GenPerm].contramap(_.genPerm)
  val permutationGroup: Group[Permutation] = Group[GenPerm].imap(new FreeBasedPermutation[self.type, Free](_))(_.genPerm)
  val permutationFaithfulPermutationActionBuilder: FaithfulPermutationActionBuilder[Permutation] =
    FaithfulPermutationActionBuilder[GenPerm].contramap(_.genPerm)
  val permutationMonoAction: Action[Monomial, Permutation] = new FreeBasedPermutation.FreeBasedPermutationMonoAction[self.type, Free]

  /** Returns the symmetry group that leaves the structure of this monoid invariant. */
  def symmetryGroup: Grp[FreeBasedPermutation[self.type, Free]]

  //endregion

}

object FreeBasedMonoidDef {
  type Aux[F <: free.MonoidDef with Singleton] = FreeBasedMonoidDef { type Free = F }

  private[FreeBasedMonoidDef] final class PolyInstances[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](implicit val wM: Witness.Aux[M])
    extends FieldAssociativeAlgebra[FreeBasedPoly[M, F], Cyclo] with Involution[FreeBasedPoly[M, F]] with Eq[FreeBasedPoly[M, F]] {
    def adjoint(a: FreeBasedPoly[M, F]): FreeBasedPoly[M, F] = a.adjoint
    def negate(x: FreeBasedPoly[M, F]): FreeBasedPoly[M, F] = -x
    def zero: FreeBasedPoly[M, F] = FreeBasedPoly.zero[M, F]
    def plus(x: FreeBasedPoly[M, F], y: FreeBasedPoly[M, F]): FreeBasedPoly[M, F] = x + y
    def one: FreeBasedPoly[M, F] = FreeBasedPoly.one[M, F]
    def times(x: FreeBasedPoly[M, F], y: FreeBasedPoly[M, F]): FreeBasedPoly[M, F] = x * y
    def eqv(x: FreeBasedPoly[M, F], y: FreeBasedPoly[M, F]): Boolean = x == y
    implicit def scalar: Field[Cyclo] = Cyclo.typeclasses
    def timesl(c: Cyclo, v: FreeBasedPoly[M, F]): FreeBasedPoly[M, F] = c *: v
  }

}
