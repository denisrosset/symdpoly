package net.alasc.symdpoly

package object joptimizer {

  implicit class JOptimizerRelaxation(val relaxation: OldRelaxation[_ ,_]) {
    def jOptimizerInstance: OldJOptimizerInstance = new OldJOptimizerInstance(relaxation)
  }

  implicit class JOptimizerSDP(val sdp: SDP) {
    def jOptimizer: JOptimizerInstance = new JOptimizerInstance(sdp)
  }

}
