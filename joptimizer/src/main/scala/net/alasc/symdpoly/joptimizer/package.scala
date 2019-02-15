package net.alasc.symdpoly

package object joptimizer {

  implicit class JOptimizerRelaxation(val relaxation: Relaxation[_ ,_]) {
    def jOptimizerInstance: JOptimizerInstance = new JOptimizerInstance(relaxation)
  }

}
