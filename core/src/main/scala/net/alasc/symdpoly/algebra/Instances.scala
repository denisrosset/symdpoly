package net.alasc.symdpoly.algebra

import cats.{Contravariant, Invariant}
import spire.algebra.{Field, VectorSpace}
import spire.math.SafeLong

import algebra.{CommutativeGroup, Eq}
import net.alasc.algebra.PermutationAction
import net.alasc.perms.Perm
import net.alasc.util.NNOption
import cats.instances.eq.catsContravariantMonoidalForEq

import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}

object Instances {

  implicit val symdpolyContravariantForPermutationAction: Contravariant[PermutationAction] = new Contravariant[PermutationAction] {
    def contramap[A, B](fa: PermutationAction[A])(f: B => A): PermutationAction[B] = new PermutationAction[B] {
      def isFaithful: Boolean = fa.isFaithful
      override def movesAnyPoint(g: B): Boolean = fa.movesAnyPoint(f(g))
      override def movesPoint(g: B, i: Int): Boolean = fa.movesPoint(f(g), i)
      override def nMovedPoints(g: B): Int = fa.nMovedPoints(f(g))
      override def movedPoints(g: B): Set[Int] = fa.movedPoints(f(g))
      override def largestMovedPoint(g: B): NNOption = fa.largestMovedPoint(f(g))
      override def smallestMovedPoint(g: B): NNOption = fa.smallestMovedPoint(f(g))
      override def signPerm(g: B): Int = fa.signPerm(f(g))
      override def cycleStructure(g: B): Map[Int, Int] = fa.cycleStructure(f(g))
      override def permutationOrder(g: B): SafeLong = fa.permutationOrder(f(g))
      override def orbit(g: B, i: Int): Set[Int] = fa.orbit(f(g), i)
      override def images(g: B, n: Int): Seq[Int] = fa.images(f(g), n)
      override def toPerm(g: B): Perm = fa.toPerm(f(g))
      override def hasSameAction[Q](g: B, q: Q)(implicit Q: PermutationAction[Q]): Boolean = fa.hasSameAction(f(g), q)
      override def smallestMovedPoint(generators: Iterable[B]): NNOption = fa.smallestMovedPoint(generators.map(f))
      override def largestMovedPoint(generators: Iterable[B]): NNOption = fa.largestMovedPoint(generators.map(f))
      def findMovedPoint(g: B): NNOption = fa.findMovedPoint(f(g))
      def movedPointsUpperBound(g: B): NNOption = fa.movedPointsUpperBound(f(g))
      def actr(p: Int, g: B): Int = fa.actr(p, f(g))
      def actl(g: B, p: Int): Int = fa.actl(f(g), p)
    }
  }

  implicit def symdpolyInvariantForVectorSpace[F]: Invariant[Lambda[V => VectorSpace[V, F]]] = new Invariant[Lambda[V => VectorSpace[V, F]]] {
    def imap[A, B](fa: VectorSpace[A, F])(f: A => B)(g: B => A): VectorSpace[B, F] = new VectorSpace[B, F] {
      override def divr(v: B, s: F): B = f(fa.divr(g(v), s))
      override def timesr(v: B, s: F): B = f(fa.timesr(g(v), s))
      override def minus(x: B, y: B): B = f(fa.minus(g(x), g(y)))
      override def sumN(b: B, n: Int): B = f(fa.sumN(g(b), n))
      override def isZero(b: B)(implicit ev: Eq[B]): Boolean = fa.isZero(g(b))(Eq.by[A, B](f))
      override def sum(bs: TraversableOnce[B]): B = f(fa.sum(bs.map(g)))
      override def trySum(bs: TraversableOnce[B]): Option[B] = fa.trySum(bs.map(g)).map(f)
      implicit def scalar: Field[F] = fa.scalar
      def timesl(s: F, v: B): B = f(fa.timesl(s, g(v)))
      def negate(x: B): B = f(fa.negate(g(x)))
      def zero: B = f(fa.zero)
      def plus(x: B, y: B): B = f(fa.plus(g(x), g(y)))
    }
  }

  implicit val symdpolyContravariantForFaithfulPermutationActionBuilder: Contravariant[FaithfulPermutationActionBuilder] = new Contravariant[FaithfulPermutationActionBuilder] {
    def contramap[A, B](fa: FaithfulPermutationActionBuilder[A])(f: B => A): FaithfulPermutationActionBuilder[B] = new FaithfulPermutationActionBuilder[B] {
      def apply(generators: Iterable[B]): PermutationAction[B] = {
        val permutationActionA = fa.apply(generators.map(f))
        Contravariant[PermutationAction].contramap[A, B](permutationActionA)(f)
      }
    }
  }

}
