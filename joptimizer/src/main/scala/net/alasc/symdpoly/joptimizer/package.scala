package net.alasc.symdpoly

package object joptimizer {

  implicit class JOptimizerRelaxation(val relaxation: OldRelaxation[_ ,_]) {
    def jOptimizerInstance: OldJOptimizerInstance = new OldJOptimizerInstance(relaxation)
  }

}
