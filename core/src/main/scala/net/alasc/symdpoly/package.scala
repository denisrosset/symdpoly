package net.alasc

import cyclo.Cyclo
import shapeless.Witness
import spire.algebra.{Action, AdditiveGroup}
import scalin.SparseAdditiveGroup

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

}
