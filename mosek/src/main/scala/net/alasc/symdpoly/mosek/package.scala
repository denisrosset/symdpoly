package net.alasc.symdpoly

package object mosek {

  implicit class RelaxationMosekOps(val relaxation: Relaxation[_, _, _]) extends AnyVal {

    def mosekInstance: net.alasc.symdpoly.mosek.MosekInstance =
      new net.alasc.symdpoly.mosek.MosekInstance(relaxation)

  }

}
