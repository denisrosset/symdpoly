package net.alasc.symdpoly.evaluation

import scala.annotation.tailrec

import cats.{Contravariant, Invariant}
import shapeless.Witness
import spire.algebra.{Action, Eq, Order, VectorSpace}
import spire.syntax.action._

import cyclo.Cyclo

import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.generic
import cats.instances.order.catsContravariantMonoidalForOrder
import cats.instances.eq.catsContravariantMonoidalForEq

import net.alasc.finite.Grp
import net.alasc.symdpoly.algebra.Instances._

trait Evaluator2[M <: generic.MonoidDef with Singleton] { self =>

  val witness: Witness.Aux[self.type] = Witness.mkWitness(self)

  def M: M

  // optimization: set to true if apply(a) == apply(a.adjoint)
  def isSelfAdjoint: Boolean

  def apply(mono: M#Monomial): EvaluatedMono2[self.type, M]

  def apply(poly: M#Polynomial)(implicit d: DummyImplicit): EvaluatedPoly2[self.type, M] = {
    implicit def wM: Witness.Aux[M] = M.witness
    @tailrec def iter(i: Int, acc: M#Polynomial): M#Polynomial =
      if (i == poly.nTerms) acc else {
        val newTerm = M.polyAssociativeAlgebra.timesl(poly.coeff(i), M.monomialToPolynomial(apply(poly.monomial(i)).normalForm))
        iter(i + 1, M.polyAssociativeAlgebra.plus(acc, newTerm))
      }
    new EvaluatedPoly2[self.type, M](iter(0, M.polyAssociativeAlgebra.zero))
  }

  // additional equivalences due to

  def symmetrize[G](symmetryGroup: Grp[G])(implicit action: Action[EvaluatedMono2[self.type, M], G]): Evaluator2[M] = {
    implicit def wM: Witness.Aux[M] = M.witness
    new Evaluator2[M] { symSelf =>
      def M: M = self.M
      def isSelfAdjoint: Boolean = self.isSelfAdjoint
      def apply(mono: M#Monomial): EvaluatedMono2[this.type, M] = {
        val reduced = self(mono)
        import spire.syntax.std.seq._
        val minimal = symmetryGroup.iterator.toIterable.map(g => reduced <|+| g).qmin
        new EvaluatedMono2[symSelf.type, M](minimal.normalForm)
      }
    }
  }

  // typeclasses

  val evaluatedMonoOrder: Order[EvaluatedMono2[self.type, M]] = Contravariant[Order].contramap(M.monoOrder)(em => em.normalForm)
  val evaluatedMonoPhased: Phased[EvaluatedMono2[self.type, M]] = Invariant[Phased].imap(M.monoPhased)(apply)(_.normalForm)
  val evaluatedPolyEq: Eq[EvaluatedPoly2[self.type, M]] = Contravariant[Eq].contramap(M.polyEq)(ep => ep.normalForm)
  val evaluatedPolyVectorSpace: VectorSpace[EvaluatedPoly2[self.type, M], Cyclo] = Invariant[Lambda[V => VectorSpace[V, Cyclo]]].imap(M.polyAssociativeAlgebra)(apply)(_.normalForm)

}
