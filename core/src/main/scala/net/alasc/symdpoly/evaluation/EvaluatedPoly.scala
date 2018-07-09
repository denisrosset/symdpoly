package net.alasc.symdpoly
package evaluation

import shapeless.Witness
import spire.algebra._

import cyclo.Cyclo
import scalin.immutable.{Vec, VecEngine}
import scalin.syntax.all._
import spire.syntax.cfor._

import net.alasc.finite.Grp
import net.alasc.symdpoly.generic.FreeBasedMonoidDef
import net.alasc.symdpoly.math.GenPerm
import net.alasc.symdpoly.{OrderedSet, free, generic}

final class EvaluatedPoly[
  E <: Evaluator[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux,
  G <: Grp[GenPerm] with Singleton:Witness.Aux
](val normalForm: M#Polynomial) { lhs =>

  def E: E = valueOf[E]
  def G: G = valueOf[G]

  override def toString: String = normalForm.string("L(", ")")
  override def hashCode: Int = normalForm.hashCode()
  override def equals(any: Any): Boolean = any match {
    case rhs: EvaluatedPoly[E, M, G] if (lhs.E eq rhs.E) && (lhs.G eq rhs.G) =>
      valueOf[M].polyEq.eqv(lhs.normalForm, rhs.normalForm)
    case _ => false
  }

  // replace M#Monomial by EvaluatedMono over the same group
  def vecOverOrderedSet(orderedSet: OrderedSet[M#Monomial])(implicit V: VecEngine[Cyclo]): Vec[Cyclo] =
    V.fromMutable(orderedSet.length, Cyclo.zero) { vec =>
      implicit def monoOrder: Order[M#Monomial] = valueOf[M].monoOrder
      cforRange(0 until normalForm.nTerms) { i =>
        val mono = normalForm.monomial(i)
        val coeff = normalForm.coeff(i)
        val j = orderedSet.indexOf(mono)
        if (j == -1) sys.error(s"Monomial $mono not present in the moment set ${orderedSet}.") else { vec(j) := vec(j) + coeff }
      }
    }

}

object EvaluatedPoly {

  implicit class EvaluatedPolyTrivialGroup[
    E <: FreeBasedEvaluator[M, F] with Singleton:Witness.Aux,
    M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux
  ](val evaluatedPoly: EvaluatedPoly[E, M with generic.FreeBasedMonoidDef.Aux[F], M#TrivialGroup]) {

    def maximize(): Maximization[E, M, F] = Maximization(evaluatedPoly)

  }

  implicit def equ[
    E <: Evaluator[M] with Singleton:Witness.Aux,
    M <: generic.MonoidDef with Singleton,
    G <: Grp[GenPerm] with Singleton
  ]: Eq[EvaluatedPoly[E, M, G]] = Eq.fromUniversalEquals[EvaluatedPoly[E, M ,G]]

  implicit def vectorSpace[
    E <: Evaluator[M] with Singleton:Witness.Aux,
    M <: generic.MonoidDef with Singleton:Witness.Aux,
    G <: Grp[GenPerm] with Singleton:Witness.Aux
  ]: VectorSpace[EvaluatedPoly[E, M, G], Cyclo] = new EvaluatedPolyVectorSpace[E, M, G]

  implicit def genPermAction[
    E <: FreeBasedEvaluator[M, F] with Singleton:Witness.Aux,
    M <: FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux,
    G <: Grp[GenPerm] with Singleton:Witness.Aux
  ]: Action[EvaluatedPoly[E, M, G], GenPerm] =
    new EvaluatedPolyGenPermAction[E, M, F, G]
}

final class EvaluatedPolyVectorSpace[
  E <: Evaluator[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux,
  G <: Grp[GenPerm] with Singleton: Witness.Aux
] extends VectorSpace[EvaluatedPoly[E, M, G], Cyclo] {

  implicit def scalar: Field[Cyclo] = Cyclo.typeclasses
  def M: M = valueOf[M]

  def timesl(r: Cyclo, v: EvaluatedPoly[E, M, G]): EvaluatedPoly[E, M, G] =
    new EvaluatedPoly[E, M, G](M.polyAssociativeAlgebra.timesl(r, v.normalForm))

  def negate(x: EvaluatedPoly[E, M, G]): EvaluatedPoly[E, M, G] =
    new EvaluatedPoly[E, M, G](M.polyAssociativeAlgebra.negate(x.normalForm))

  def zero: EvaluatedPoly[E, M, G] = new EvaluatedPoly[E, M, G](M.polyAssociativeAlgebra.zero)

  def plus(x: EvaluatedPoly[E, M, G], y: EvaluatedPoly[E, M, G]): EvaluatedPoly[E, M, G] =
    new EvaluatedPoly[E, M, G](M.polyAssociativeAlgebra.plus(x.normalForm, y.normalForm))

}

final class EvaluatedPolyGenPermAction[
  E <: FreeBasedEvaluator[M, F] with Singleton: Witness.Aux,
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton,
  G <: Grp[GenPerm] with Singleton:Witness.Aux
](implicit actionP: Action[Poly[M, F], GenPerm]) extends Action[EvaluatedPoly[E, M, G], GenPerm]  {
  def E: E = valueOf[E]
  def actl(g: GenPerm, p: EvaluatedPoly[E, M, G]): EvaluatedPoly[E, M, G] = actr(p, g.inverse)
  def actr(p: EvaluatedPoly[E, M, G], g: GenPerm): EvaluatedPoly[E, M, G] = E(actionP.actr(p.normalForm, g), valueOf[G])
}

