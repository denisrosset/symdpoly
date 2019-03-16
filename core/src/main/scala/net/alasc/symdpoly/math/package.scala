package net.alasc.symdpoly

import spire.algebra.{AdditiveMonoid, Eq, Field, Group, Monoid, Ring}
import spire.util.Opt

import cyclo.Cyclo
import scalin.{Pivot, Sparse}
import scalin.algos.Inverse
import scalin.immutable.{Mat, MatEngine}
import spire.std.double._

package object math {

  implicit val doublePivot: Pivot[Double] = Pivot.double(1e-12)

  implicit val cycloPivot: Pivot[Cyclo] = new Pivot[Cyclo] {
    def priority(a: Cyclo): Double = cycloValue(a).absSquare
    def closeToZero(a: Cyclo): Boolean = a.isZero
    def optionalExactEq: Opt[Eq[Cyclo]] = Opt(Cyclo.typeclasses)
  }

  implicit val inverseCyclo: Inverse[Cyclo, Mat[Cyclo]] = new Inverse[Cyclo, Mat[Cyclo]] {
    import scalin.immutable.dense._
    def apply(mat: scalin.Mat[Cyclo]): Mat[Cyclo] = scalin.algos.Inverse.denseInverse[Cyclo, scalin.immutable.DenseMat[Cyclo]].apply(mat)
  }

  implicit val inverseDouble: Inverse[Double, Mat[Double]] = new Inverse[Double, Mat[Double]] {
    import scalin.immutable.dense._
    def apply(mat: scalin.Mat[Double]): Mat[Double] = scalin.algos.Inverse.denseInverse[Double, scalin.immutable.DenseMat[Double]].apply(mat)
  }

  def matGroup[A:MatEngine:Field:Lambda[A => Inverse[A, Mat[A]]]](n: Int): Group[Mat[A]] = new Group[Mat[A]] {
    def empty: Mat[A] = Mat.zeros(n, n)
    def combine(x: Mat[A], y: Mat[A]): Mat[A] = x * y
    def inverse(a: Mat[A]): Mat[A] = a.inverse
  }

}
