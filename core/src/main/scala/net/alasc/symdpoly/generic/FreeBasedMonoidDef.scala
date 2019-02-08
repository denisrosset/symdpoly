package net.alasc.symdpoly
package generic

import cyclo.Cyclo
import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp
import net.alasc.partitions.Partition
import net.alasc.symdpoly.algebra.{MultiplicativeBinoid, Phased}
import net.alasc.symdpoly.free._
import net.alasc.symdpoly.math.GenPerm
import net.alasc.perms.default._
import net.alasc.util._
import shapeless.Witness
import spire.algebra.{Action, Eq, Field, FieldAssociativeAlgebra, Involution, Order}

/** Monoid whose elements are represented by normal forms in a free monoid.
  * Is not necessarily a strict quotient monoid, as free.MonoidDef inherits from
  * this; then code can be reused between free monoids and their quotients.
  */
abstract class FreeBasedMonoidDef extends generic.MonoidDef { self =>

  // Free monoid
  type Free <: free.MonoidDef with Singleton { type Free = self.Free }
  def Free: Free
  implicit def witnessFree: Witness.Aux[Free] = Free.witness

  // Monomials

  type Monomial = Mono[self.type, Free]
  def quotient(poly: Poly[Free, Free]): Poly[self.type, Free]
  def quotient(word: Mono[Free, Free]): Monomial
  def quotient(gset: GSet[Free]): GSet[self.type] = GSet.Quotient[self.type, Free](gset)

  def monomialToPolynomial(m: Mono[self.type, Free]): Poly[self.type, Free] = Poly[self.type, Free](m)

  private[this] val monoInstances: MonoInstances[self.type, Free] = new MonoInstances[self.type, Free]
  def monoMultiplicativeBinoid: MultiplicativeBinoid[Monomial] = monoInstances
  def monoInvolution: Involution[Monomial] = monoInstances
  def monoOrder: Order[Monomial] = monoInstances
  val monoPhased: Phased[Monomial] = new MonoPhased
  val monoGenPermAction: Action[Monomial, GenPerm] = new MonoGenPermAction

  def inPlaceNormalForm(word: MutableWord[Free], start: Int = 0): Boolean

  val zero: Monomial = Mono.zero[self.type, Free]
  val one: Monomial = Mono.one[self.type, Free]

  // Polynomials

  type Polynomial = Poly[self.type, Free]

  private[this] val polyInstances: PolyInstances[self.type, Free] = new PolyInstances[self.type, Free]
  def polyAssociativeAlgebra: FieldAssociativeAlgebra[Polynomial, Cyclo] = polyInstances
  def polyInvolution: Involution[Polynomial] = polyInstances
  def polyEq: Eq[Polynomial] = polyInstances
  val polyGenPermAction: Action[Poly[self.type, Free], GenPerm] = new PolyGenPermAction

  def symmetryGroup(nRootsOfUnity: Int): Grp[GenPerm] = {
    val m = nRootsOfUnity
    val n = Free.nOperators
    def op(i: Int): Free#Op = Free.opFromIndex(i)
    def monoFromOpIndex(i: Int): Mono[Free, Free] = Mono(op(i))
    val phases: Vector[Phase] = Vector.tabulate(m)(k => Phase(k, m))
    val monos1: Vector[Mono[Free, Free]] = Vector.tabulate(n)(i => monoFromOpIndex(i))
    val monos2: Vector[Mono[Free, Free]] = Vector.tabulate(n, n)( (i, j) => Mono(op(i), op(j)) ).flatten
    val monos: Vector[Mono[Free, Free]] = Vector(Mono.one[Free, Free]) ++ (monos1 ++ monos2).flatMap(m => phases.map(p => m * p))
    val monoSet: OrderedSet[Mono[Free, Free]] = OrderedSet.fromUnique(monos)
    val action = new PermutationAction[GenPerm] {
      def isFaithful: Boolean = true
      def findMovedPoint(g: GenPerm): NNOption = g.largestMovedPoint match {
        case NNOption(i) => NNSome(monoSet.indexOf(monoFromOpIndex(i)))
        case _ => NNNone
      }
      def movedPointsUpperBound(g: GenPerm): NNOption = NNSome(monoSet.length - 1)
      def actl(g: GenPerm, i: Int): Int = actr(i, g.inverse)
      def actr(i: Int, g: GenPerm): Int = monoSet.indexOf(Free.monoGenPermAction.actr(monoSet(i), g))
    }
    val grp = GenPerm.generalizedSymmetricGroup(m, n)
    val normalForms = monoSet.iterator.map(self.quotient(_)).toVector
    val partition = Partition.fromSeq(normalForms)
    grp.unorderedPartitionStabilizer(action, partition)
  }

  def ambientGroup(generators: Generator[Free]*): Grp[GenPerm] =
    Grp.fromGenerators(generators.map(_.opAction))

}

object FreeBasedMonoidDef {
  type Aux[F <: free.MonoidDef with Singleton] = FreeBasedMonoidDef { type Free = F }
}

final class PolyInstances[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](implicit val wM: Witness.Aux[M])
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
