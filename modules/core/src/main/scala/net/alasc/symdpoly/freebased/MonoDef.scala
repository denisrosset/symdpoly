package net.alasc.symdpoly
package freebased

import scala.annotation.tailrec
import scala.reflect.ClassTag

import cats.instances.eq._
import cats.syntax.contravariant._
import cats.syntax.invariant._
import shapeless.Witness
import spire.algebra.{free => _, _}
import spire.math.Rational
import spire.syntax.eq._

import cyclo.Cyclo

import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.symdpoly.algebra.{MultiplicativeBinoid, Phased}
import net.alasc.symdpoly.free._
import net.alasc.symdpoly.math.{GenPerm, GenPermFaithfulPermutationAction, Phase}
import instances.all._
import cats.syntax.traverse._
import spire.syntax.cfor._
import cats.instances.vector._
import cats.instances.option._

import net.alasc.util._
import net.alasc.algebra.PermutationAction
import net.alasc.symdpoly.evaluation.parts.OpPartition
import net.alasc.symdpoly.evaluation.{EigenvalueEvaluator, Evaluator, FreeBasedEigenvalueEvaluator, TraceEvaluator}
import net.alasc.symdpoly.util.OrderedSet

/** Monoid whose elements are represented by normal forms in a free monoid.
  * Is not necessarily a strict quotient monoid, as free.MonoidDef inherits from
  * this; then code can be reused between free monoids and their quotients.
  */
abstract class MonoDef extends generic.MonoDef {
  self =>

  //region Members to implement

  /** Free monoid [[free.MonoDef]] on which this monoid is based. */
  type Free <: free.MonoDef with Singleton {type Free = self.Free}

  /** Free monoid instance. */
  def Free: Free

  /** Witness of the free monoid instance. */
  implicit def witnessFree: Witness.Aux[Free] = Free.witness

  def inPlaceNormalForm(word: MutableWord[Free], start: Int = 0): Boolean

  /** Quotient map from the free monoid to the quotient monoid. */
  def quotient(word: Mono[Free, Free]): MonoType

  /** Quotient map from the polynomial ring on the free monoid to the polynomial ring on the quotient monoid. */
  def quotient(poly: Poly[Free, Free]): Poly[self.type, Free]

  //endregion

  //region Monomials

  /** Element of this monoid, i.e. a monomial. */
  type MonoType = Mono[self.type, Free]

  /** Quotient map applied to generating sets of monomials. */
  def quotient(gset: GSet[Free]): GSet[self.type] = GSet.Quotient[self.type, Free](gset)

  // Monomial typeclass instances

  private[this] val monoInstances: Mono.FreeBasedMonoInstances[self.type, Free] = new Mono.FreeBasedMonoInstances[self.type, Free]

  def monoMultiplicativeBinoid: MultiplicativeBinoid[MonoType] = monoInstances

  def monoInvolution: Involution[MonoType] = monoInstances

  def monoOrder: Order[MonoType] = monoInstances

  val monoPhased: Phased[MonoType] = new Mono.FreeBasedMonoPhased
  val monoGenPermAction: Action[MonoType, GenPerm] = new Mono.FreeBasedMonoGenPermAction
  val monoClassTag: ClassTag[MonoType] = implicitly

  val zero: MonoType = Mono.zero[self.type, Free]
  val one: MonoType = Mono.one[self.type, Free]

  //endregion

  //region Polynomials

  /** Polynomial are elements of the ring defined on this monoid. */
  type PolyType = Poly[self.type, Free]

  /** Returns a polynomial containing a single monomial term. */
  def monomialToPolynomial(m: Mono[self.type, Free]): Poly[self.type, Free] = Poly[self.type, Free](m)

  // Polynomial typeclass instances

  private[this] val polyInstances: PolyInstances[self.type, Free] = new PolyInstances[self.type, Free]

  def polyAssociativeAlgebra: FieldAssociativeAlgebra[PolyType, Cyclo] = polyInstances

  def polyInvolution: Involution[PolyType] = polyInstances

  def polyEq: Eq[PolyType] = polyInstances

  def permutationPolyAction: Action[PolyType, PermutationType] = polyInstances

  val polyClassTag: ClassTag[PolyType] = implicitly

  def constant(i: Int): PolyType = polyAssociativeAlgebra.fromInt(i)

  def constant(r: Rational): PolyType = polyAssociativeAlgebra.timesl(r, polyAssociativeAlgebra.one)

  def constant(c: Cyclo): PolyType = polyAssociativeAlgebra.timesl(c, polyAssociativeAlgebra.one)

  //endregion

  //region Permutations

  type PermutationType = freebased.Permutation[self.type, Free]

  object Permutation {
    /** Constructs a permutation from a generalized permutation of the free operators.
      *
      * Does not check whether the resulting permutation is compatible with the monoid structure.
      */
    def applyNC(genPerm: GenPerm): PermutationType = new freebased.Permutation[self.type, Free](genPerm)
  }

  val permutationEq: Eq[PermutationType] = Eq[GenPerm].contramap(_.genPerm)
  val permutationGroup: Group[PermutationType] = Group[GenPerm].imap(new freebased.Permutation[self.type, Free](_))(_.genPerm)
  lazy val permutationFaithfulPermutationAction: PermutationAction[PermutationType] =
    contravariantForFaithfulPermutationAction.contramap(GenPermFaithfulPermutationAction(Free.nOperators, cyclotomicOrder))(_.genPerm)
  val permutationFaithfulPermutationActionBuilder: FaithfulPermutationActionBuilder[PermutationType] = new FaithfulPermutationActionBuilder[PermutationType] {
    def apply(generators: Iterable[PermutationType]): PermutationAction[PermutationType] = permutationFaithfulPermutationAction
 }
  val permutationMonoAction: Action[MonoType, PermutationType] = new freebased.PermutationMonoAction[self.type, Free]
  val permutationClassTag: ClassTag[PermutationType] = implicitly

  /** Returns the symmetry group that leaves the structure of this monoid invariant. */
  def symmetryGroup: Grp[freebased.Permutation[self.type, Free]]

  //endregion

  //region Evaluator construction

  override def eigenvalueEvaluator(real: Boolean = false): Evaluator.Aux[this.type] =
      if (Settings.optimize) new FreeBasedEigenvalueEvaluator[this.type, Free](real, Grp.trivial[PermutationType])
      else super.eigenvalueEvaluator(real)

  /** Creates an evaluator for trace optimization.
    *
    * @param real Whether the problem is real, i.e. we have L(x) = L(x.adjoint), where L is this evaluator
    *             By default, we do not enforce the problem to be real..
    */
  def traceEvaluator(real: Boolean = false): Evaluator.Aux[this.type] =
    new TraceEvaluator[this.type, Free](real, Grp.trivial[PermutationType])
  //endregion


}

object MonoDef {

  type Aux[F <: free.MonoDef.Aux[F] with Singleton] = MonoDef { type Free = F }

}
