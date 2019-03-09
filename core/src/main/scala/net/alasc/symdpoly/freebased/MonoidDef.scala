package net.alasc.symdpoly
package freebased

import cats.instances.eq._
import cats.syntax.contravariant._
import cats.syntax.invariant._
import shapeless.Witness
import spire.algebra.{free => _, _}
import spire.math.Rational

import cyclo.Cyclo

import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.symdpoly.algebra.{MultiplicativeBinoid, Phased}
import net.alasc.symdpoly.free._
import net.alasc.symdpoly.freebased.MonoidDef.PolyInstances
import net.alasc.symdpoly.math.GenPerm
import instances.all._
import cats.syntax.traverse._
import cats.instances.vector._
import cats.instances.option._

/** Monoid whose elements are represented by normal forms in a free monoid.
  * Is not necessarily a strict quotient monoid, as free.MonoidDef inherits from
  * this; then code can be reused between free monoids and their quotients.
  */
abstract class MonoidDef extends generic.MonoidDef { self =>

  //region Members to implement

  /** Free monoid [[free.MonoidDef]] on which this monoid is based. */
  type Free <: free.MonoidDef with Singleton { type Free = self.Free }
  /** Free monoid instance. */
  def Free: Free
  /** Witness of the free monoid instance. */
  implicit def witnessFree: Witness.Aux[Free] = Free.witness

  def inPlaceNormalForm(word: MutableWord[Free], start: Int = 0): Boolean

  /** Quotient map from the free monoid to the quotient monoid. */
  def quotient(word: Mono[Free, Free]): Monomial

  /** Quotient map from the polynomial ring on the free monoid to the polynomial ring on the quotient monoid. */
  def quotient(poly: Poly[Free, Free]): Poly[self.type, Free]

  //endregion

  //region Monomials

  /** Element of this monoid, i.e. a monomial. */
  type Monomial = Mono[self.type, Free]

  /** Quotient map applied to generating sets of monomials. */
  def quotient(gset: GSet[Free]): GSet[self.type] = GSet.Quotient[self.type, Free](gset)


  // Monomial typeclass instances

  private[this] val monoInstances: Mono.FreeBasedMonoInstances[self.type, Free] = new Mono.FreeBasedMonoInstances[self.type, Free]

  def monoMultiplicativeBinoid: MultiplicativeBinoid[Monomial] = monoInstances
  def monoInvolution: Involution[Monomial] = monoInstances
  def monoOrder: Order[Monomial] = monoInstances
  val monoPhased: Phased[Monomial] = new Mono.FreeBasedMonoPhased
  val monoGenPermAction: Action[Monomial, GenPerm] = new Mono.FreeBasedMonoGenPermAction

  val zero: Monomial = Mono.zero[self.type, Free]
  val one: Monomial = Mono.one[self.type, Free]

  //endregion

  //region Polynomials

  /** Polynomial are elements of the ring defined on this monoid. */
  type Polynomial = Poly[self.type, Free]

  /** Returns a polynomial containing a single monomial term. */
  def monomialToPolynomial(m: Mono[self.type, Free]): Poly[self.type, Free] = Poly[self.type, Free](m)

  // Polynomial typeclass instances

  private[this] val polyInstances: PolyInstances[self.type, Free] = new PolyInstances[self.type, Free]
  def polyAssociativeAlgebra: FieldAssociativeAlgebra[Polynomial, Cyclo] = polyInstances
  def polyInvolution: Involution[Polynomial] = polyInstances
  def polyEq: Eq[Polynomial] = polyInstances
  val polyGenPermAction: Action[Poly[self.type, Free], GenPerm] = new Poly.PolyGenPermAction

  def constant(i: Int): Polynomial = polyAssociativeAlgebra.fromInt(i)
  def constant(r: Rational): Polynomial = polyAssociativeAlgebra.timesl(r, polyAssociativeAlgebra.one)
  def constant(c: Cyclo): Polynomial = polyAssociativeAlgebra.timesl(c, polyAssociativeAlgebra.one)

  //endregion

  //region Permutations

  type Permutation = freebased.Permutation[self.type, Free]

  val permutationEq: Eq[Permutation] = Eq[GenPerm].contramap(_.genPerm)
  val permutationGroup: Group[Permutation] = Group[GenPerm].imap(new freebased.Permutation[self.type, Free](_))(_.genPerm)
  val permutationFaithfulPermutationActionBuilder: FaithfulPermutationActionBuilder[Permutation] =
    FaithfulPermutationActionBuilder[GenPerm].contramap(_.genPerm)
  val permutationMonoAction: Action[Monomial, Permutation] = new freebased.PermutationMonoAction[self.type, Free]

  /** Returns the symmetry group that leaves the structure of this monoid invariant. */
  def symmetryGroup: Grp[freebased.Permutation[self.type, Free]]

  //endregion

  override def evaluator(equivalences: generic.Equivalence[self.type]*): generic.Evaluator[self.type] = {
    val transformed: Vector[Option[freebased.Equivalence[self.type, Free]]] = equivalences.toVector.map {
      case freeBased: freebased.Equivalence[self.type, Free] =>
        Some(freeBased)
      case adjoint: generic.AdjointEquivalence[self.type] =>
        Some(new freebased.AdjointEquivalence[self.type, Free])
      case symmetry: generic.SymmetryEquivalence[self.type, groupType] if symmetry.action.isInstanceOf[PermutationMonoAction[self.type, Free]] =>
        val grp = symmetry.grp.asInstanceOf[Grp[freebased.Permutation[self.type, Free]]]
        Some(new freebased.SymmetryEquivalence(grp))
      case _ =>
        None
    }
    val freeBasedEquivalences: Option[Vector[freebased.Equivalence[self.type, Free]]] = transformed.sequence
    freeBasedEquivalences match {
      case Some(freeBasedEqs) => new freebased.Evaluator(freeBasedEqs)
      case None => new generic.Evaluator[self.type](equivalences)
    }
    // freebased.Evaluator[self.type, Free](equivalences)
  }

}

object MonoidDef {
  type Aux[F <: free.MonoidDef.Aux[F] with Singleton] = MonoidDef { type Free = F }

  private[MonoidDef] final class PolyInstances[M <: MonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](implicit val wM: Witness.Aux[M])
    extends FieldAssociativeAlgebra[Poly[M, F], Cyclo] with Involution[Poly[M, F]] with Eq[Poly[M, F]] {
    def adjoint(a: Poly[M, F]): Poly[M, F] = a.adjoint
    def negate(x: Poly[M, F]): Poly[M, F] = -x
    def zero: Poly[M, F] = Poly.zero[M, F]
    def plus(x: Poly[M, F], y: Poly[M, F]): Poly[M, F] = x + y
    def one: Poly[M, F] = Poly.one[M, F]
    def times(x: Poly[M, F], y: Poly[M, F]): Poly[M, F] = x * y
    def eqv(x: Poly[M, F], y: Poly[M, F]): Boolean = x == y
    implicit def scalar: Field[Cyclo] = Cyclo.typeclasses
    def timesl(c: Cyclo, v: Poly[M, F]): Poly[M, F] = c *: v
  }

}
