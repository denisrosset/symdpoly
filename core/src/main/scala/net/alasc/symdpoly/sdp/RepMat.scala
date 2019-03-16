package net.alasc.symdpoly
package sdp

import scala.annotation.tailrec

import spire.algebra.{Group, MultiplicativeSemigroup}
import spire.math.Complex
import spire.std.double._

import scalin.immutable.{Mat, Vec}
import scalin.immutable.csc._

import net.alasc.symdpoly.math.GenPerm
import spire.syntax.group._

import net.alasc.symdpoly.algebra.Morphism
import net.alasc.symdpoly.sdp.GenPermMat.{ComplexMat, RealMat}

case class MatElement(r: Int, c: Int, real: Double)

case class RepMat(blocks: Seq[GenPermMat]) {
  private[this] val start = blocks.map(_.n).scanLeft(0)(_ + _).toArray
  def n: Int = start(start.length - 1)
  lazy val elements: Seq[MatElement] = for {
    (block, i) <- blocks.zipWithIndex
    s = start(i)
    MatElement(r, c, e) <- block.elements
  } yield MatElement(s + r, s + c, e)
  def toMat: Mat[Double] =
    Mat.sparse(n, n)(Vec(elements.map(_.r): _*), Vec(elements.map(_.c):_*), Vec(elements.map(_.real):_*))

}

object RepMat {

  def realTrivialMorphism[G:Group](n: Int): Morphism[G, RepMat, Group] = {
    val id = RepMat(Seq(RealMat(n, GenPerm.id)))
    Morphism[G, RepMat, Group](x => id)(implicitly, groupInstance(id))
  }

  def groupInstance(identity: RepMat): Group[RepMat] = new Group[RepMat] {
    def inverse(mat: RepMat): RepMat = RepMat(mat.blocks.map {
      case RealMat(n, g) => RealMat(n, g.inverse)
      case ComplexMat(n, g) => ComplexMat(n, g.inverse)
    })
    def empty: RepMat = identity
    def combine(x: RepMat, y: RepMat): RepMat = RepMat((x.blocks zip y.blocks).map {
      case (RealMat(n1, g1), RealMat(n2, g2)) if n1 == n2 => RealMat(n1, g1 |+| g2)
      case (ComplexMat(n1, g1), ComplexMat(n2, g2)) if n1 == n2 => ComplexMat(n1, g1 |+| g2)
      case _ => sys.error("Invalid block type")
    })
  }

}

sealed trait GenPermMat {
  def n: Int
  def elements: Seq[MatElement]
  def toMat: Mat[Double] =
    Mat.sparse(n, n)(Vec(elements.map(_.r): _*), Vec(elements.map(_.c):_*), Vec(elements.map(_.real):_*))
}

object GenPermMat {

  case class RealMat(n: Int, g: GenPerm) extends GenPermMat {
    def elements: Seq[MatElement] = for {
      c <- 0 until n
      r = g.perm.image(c)
      p = g.phases.phaseFor(r).toInt.toDouble // calling toDouble directly would give NaN
    } yield MatElement(r, c, p)
  }

  def encoding(f: Complex[Double], r0: Int, c0: Int): Double =
    if (r0 == c0) f.real
    else if (r0 == 0) -f.imag
    else f.imag

  case class ComplexMat(n: Int, g: GenPerm) extends GenPermMat {
    def elements: Seq[MatElement] = for {
      c <- 0 until n/2
      r = g.perm.image(c)
      r0 <- Seq(0, 1)
      c0 <- Seq(0, 1)
      f = phaseValue(g.phases.phaseFor(r))
      e = encoding(f, r0, c0) if e != 0
    } yield MatElement(r * 2 + r0, c * 2 + c0, e)
  }

}
