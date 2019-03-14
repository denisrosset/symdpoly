package net.alasc.symdpoly

package object joptimizer {

  implicit class JOptimizerSDP(val sdp: SDP) {
    def jOptimizer: JOptimizerInstance = new JOptimizerInstance(sdp)
  }

}
