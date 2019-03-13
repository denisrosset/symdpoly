package net.alasc

import cyclo.{Cyclo, RealCyclo}

import net.alasc.finite.Grp
import shapeless.Witness
import spire.algebra.{Action, AdditiveGroup}

import scalin.SparseAdditiveGroup
import scala.collection.mutable

import spire.std.double._
import spire.math.Complex

import net.alasc.symdpoly.math.Phase
import net.alasc.symdpoly.util.MemoMap

package object symdpoly {

  def trivialAction[A]: Action[A, Unit] = new Action[A, Unit] {
    def actl(g: Unit, a: A): A = a
    def actr(a: A, g: Unit): A = a
  }

  implicit def cycloFromInt(i: Int): Cyclo = Cyclo(i)

  implicit val sparseDouble: SparseAdditiveGroup[Double] = new SparseAdditiveGroup[Double] {
    def zero: Double = 0.0
    def provenZero(a: Double): Boolean = a == 0.0
    def additive: AdditiveGroup[Double] = spire.std.double.DoubleAlgebra
  }

  @inline def valueOf[S <: Singleton](implicit wS: Witness.Aux[S]): S = wS.value

  private[this] val cachedCycloDoubleValues: mutable.HashMap[Cyclo, Double] = mutable.HashMap.empty

  private[this] def computeCycloToDouble(cyclo: Cyclo): Double =
    if (cyclo.isRational) cyclo.toRational.toDouble
    else RealCyclo.real(cyclo).toAlgebraic.toDouble

  /** Computes a Double approximation of a real cyclotomic number. */
  def realCycloToDouble(cyclo: Cyclo): Double =
    cachedCycloDoubleValues.getOrElseUpdate(cyclo, computeCycloToDouble(cyclo))

  val cycloValue = MemoMap[Cyclo, Complex[Double]](c => Complex(RealCyclo.real(c).toAlgebraic.toDouble, RealCyclo.imag(c).toAlgebraic.toDouble))
  val phaseValue = MemoMap[Phase, Complex[Double]](p => cycloValue(p.toCyclo))

}
