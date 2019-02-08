package net.alasc.symdpoly

package object joptimizer {

  implicit class JOptimizerRelaxation(val relaxation: Relaxation[_,_,_]) {
    def jOptimizerInstance: JOptimizerInstance = new JOptimizerInstance(relaxation)
  }

  implicit class JOptimizerRelaxation2(val relaxation: Relaxation2[_ ,_]) {
    def jOptimizerInstance: JOptimizerInstance2 = new JOptimizerInstance2(relaxation)
  }

}
