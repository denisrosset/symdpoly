package net.alasc

import shapeless.Witness
import spire.algebra.AdditiveGroup

import scalin.SparseAdditiveGroup

package object symdpoly {

  implicit val sparseDouble: SparseAdditiveGroup[Double] = new SparseAdditiveGroup[Double] {
    def zero: Double = 0.0
    def provenZero(a: Double): Boolean = a == 0.0
    def additive: AdditiveGroup[Double] = spire.std.double.DoubleAlgebra
  }

  @inline def valueOf[S <: Singleton](implicit wS: Witness.Aux[S]): S = wS.value

  implicit def witnessTrivialGroup[M <: generic.MonoidDef with Singleton:Witness.Aux]: Witness.Aux[M#TrivialGroup] =
    valueOf[M].trivialGroupWitness

}
