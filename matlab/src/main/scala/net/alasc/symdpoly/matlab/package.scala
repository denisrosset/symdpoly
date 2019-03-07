package net.alasc.symdpoly

import net.alasc.symdpoly.math.{GenPerm, Phase, PhasedInt}

package object matlab {

  implicit class MatlabRelaxation(val relaxation: Relaxation[_, _]) {
    def scsInstance: SCSInstance = new SCSInstance(relaxation)
    def sdpt3Instance: SDPT3Instance = new SDPT3Instance(relaxation)
    def sedumiInstance: SeDuMiInstance = new SeDuMiInstance(relaxation)
  }

  implicit class MatlabGenPerm(val genPerm: GenPerm) extends AnyVal {
    /** Computes the signed permutation on 1-based indices, for the domain {-n...-1 1...n} */
    def matlabImage(n: Int): Array[Int] = {
      Array.tabulate[Int](n) { pIndex =>
        genPerm.image(PhasedInt(Phase.one, pIndex)) match {
          case PhasedInt(Phase.one, iIndex) => iIndex + 1
          case PhasedInt(Phase.minusOne, iIndex) => -(iIndex + 1)
        }
      }
    }
  }

}
