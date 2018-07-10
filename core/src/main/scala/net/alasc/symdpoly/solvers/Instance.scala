package net.alasc.symdpoly
package solvers

import scala.collection.mutable

import cyclo.{Cyclo, RealCyclo}

abstract class Instance {

  def relaxation: Relaxation[_, _, _]

  require(relaxation.gramMatrix.momentSet(0).isOne, "Error: empty/one monomial not part of the relaxation")

  private[this] val cachedCycloDoubleValues: mutable.HashMap[Cyclo, Double] = mutable.HashMap.empty

  private[this] def computeCycloToDouble(cyclo: Cyclo): Double =
    if (cyclo.isRational) cyclo.toRational.toDouble
    else RealCyclo.real(cyclo).toAlgebraic.toDouble

  def cycloToDouble(cyclo: Cyclo): Double =
    cachedCycloDoubleValues.getOrElseUpdate(cyclo, computeCycloToDouble(cyclo))

}
