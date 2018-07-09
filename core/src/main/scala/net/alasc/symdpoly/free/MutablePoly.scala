package net.alasc.symdpoly
package free

import cyclo.Cyclo
import metal.mutable.{HashMap => MMap}
import metal.syntax._
import net.alasc.symdpoly.generic.FreeBasedMonoidDef
import shapeless.Witness

import scala.annotation.tailrec

class MutablePoly[F <: free.MonoidDef.Aux[F] with Singleton](private[this] val _terms: MMap[MutableWord[F], Cyclo])(implicit val wF: Witness.Aux[F]) { lhs =>

  def copy: MutablePoly[F] = new MutablePoly[F](_terms.mutableCopy)

  def F: F = wF.value

  def nTerms: Int = _terms.size

  def terms: metal.generic.HashMap[MutableWord[F], Cyclo] = _terms

  def apply(monomial: MutableWord[F]): Cyclo = _terms.getOrElse(monomial, Cyclo.zero)

  def update(monomial: MutableWord[F], coeff: Cyclo): Unit = {
    require(monomial.phase.isOne)
    if (coeff.isZero)
      _terms.remove(monomial)
    else
      _terms(monomial.immutableCopy) = coeff
  }

  def add(monomial: MutableWord[F], coeff: Cyclo): MutablePoly[F] =
    if (coeff.isZero || monomial.isZero) lhs else {
      require(monomial.phase.isOne)
      val newCoeff = _terms.getOrElse(monomial, Cyclo.zero) + coeff
      if (newCoeff.isZero)
        _terms.remove(monomial)
      else
        _terms(monomial.immutableCopy) = newCoeff
      lhs
    }

  def setToZero(): MutablePoly[F] = {
    _terms.reset()
    lhs
  }
  def isZero: Boolean = (_terms.size == 0)

  override def hashCode: Int = sys.error("Not Implemented")

  def immutableCopy[M <: FreeBasedMonoidDef.Aux[F] with Singleton: Witness.Aux]: Poly[M, F] = {
    val n = _terms.size
    val keyArray = new Array[MutableWord[F]](n)
    val valueArray = new Array[Cyclo](n)
    var i = 0
    _terms.foreach { (key, value) =>
        keyArray(i) = key
        valueArray(i) = value
      i += 1
    }
    @inline def swap(i: Int, j: Int): Unit = {
      val tmpk = keyArray(i)
      val tmpv = valueArray(i)
      keyArray(i) = keyArray(j)
      valueArray(i) = valueArray(j)
      keyArray(j) = tmpk
      valueArray(j) = tmpv
    }
    @tailrec def combSortPass(previousGap: Int): Unit = {
      val gap = spire.math.max(1, (previousGap / 1.247).toInt)
      @tailrec def iter(i: Int, hasSwapped: Boolean): Boolean =
        if (i + gap >= n) hasSwapped else {
          if (keyArray(i + gap).compareToIgnoringPhase(keyArray(i)) < 0) {
            swap(i + gap, i)
            iter(i + 1, true)
          } else iter(i + 1, hasSwapped)
        }
      if (iter(0, false) || gap > 1)
        combSortPass(gap)
    }
    if (n > 1) combSortPass(n)
    new Poly[M, F](keyArray, valueArray)
  }

  override def toString: String = immutableCopy[F].toString

}

object MutablePoly {
  def empty[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](reservedSize: Int = 8): MutablePoly[F] =
    new MutablePoly[F](MMap.reservedSize(reservedSize))
}