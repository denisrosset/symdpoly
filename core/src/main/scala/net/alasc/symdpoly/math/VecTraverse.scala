package net.alasc.symdpoly.math

import cats.syntax.functor._
import cats.{CommutativeApplicative, Functor, UnorderedTraverse}
import scalin.immutable.Vec
import spire.algebra.CMonoid

final class VecTraverse extends UnorderedTraverse[Vec] with Functor[Vec] {
  implicit val instance: UnorderedTraverse[Vector] with Functor[Vector] =
    cats.instances.vector.catsStdInstancesForVector

  def unorderedTraverse[G[_]:CommutativeApplicative, A, B](sa: Vec[A])(f: A => G[B]): G[Vec[B]] =
    instance.unorderedTraverse(sa.toIndexedSeq.toVector)(f).map(scalin.immutable.DenseVec.fromSeq)

  def map[A, B](fa: Vec[A])(f: A => B): Vec[B] = scalin.immutable.DenseVec.defaultEngine[B].map(fa)(f)

  def unorderedFoldMap[A, B](fa: Vec[A])(f: A => B)(implicit evidence$1: CMonoid[B]): B =
    instance.unorderedFoldMap(fa.toIndexedSeq.toVector)(f)
}
