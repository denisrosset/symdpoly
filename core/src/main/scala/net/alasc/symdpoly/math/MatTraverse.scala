package net.alasc.symdpoly.math

import cats.syntax.functor._
import cats.{CommutativeApplicative, Functor, UnorderedTraverse}
import scalin.immutable.Mat
import spire.algebra.CMonoid

final class MatTraverse extends UnorderedTraverse[Mat] with Functor[Mat] {

  type MatAsMap[A] = Map[(Int, Int), A]

  implicit val instance: UnorderedTraverse[MatAsMap] with Functor[MatAsMap] =
    cats.instances.map.catsStdInstancesForMap[(Int, Int)]

  def toMatAsMap[A](m: Mat[A]): MatAsMap[A] = Map(
    (
      for {
        r <- 0 until m.nRows
        c <- 0 until m.nCols
      } yield ((r, c), m(r, c))
      ): _*)

  def fromMatAsMap[A](m: MatAsMap[A], nRows: Int, nCols: Int): Mat[A] =
    scalin.immutable.DenseMat.tabulate(nRows, nCols) { (r, c) => m((r, c)) }

  def unorderedTraverse[G[_]:CommutativeApplicative, A, B](sa: Mat[A])(f: A => G[B]): G[Mat[B]] =
    instance.unorderedTraverse(toMatAsMap(sa))(f).map(fromMatAsMap(_, sa.nRows, sa.nCols))

  def unorderedFoldMap[A, B:CMonoid](fa: Mat[A])(f: A => B): B =
    instance.unorderedFoldMap(toMatAsMap(fa))(f)

  def map[A, B](fa: Mat[A])(f: A => B): Mat[B] = scalin.immutable.DenseMat.defaultEngine[B].map(fa)(f)

}
