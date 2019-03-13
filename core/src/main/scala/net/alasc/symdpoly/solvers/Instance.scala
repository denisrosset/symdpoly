package net.alasc.symdpoly
package solvers

import scala.collection.mutable

import cyclo.{Cyclo, RealCyclo}

abstract class Instance {

  def relaxation: OldRelaxation[_, _]

  //require(relaxation.gramMatrix.momentSet(0).isOne, "Error: empty/one monomial not part of the relaxation")
  // TODO


}
