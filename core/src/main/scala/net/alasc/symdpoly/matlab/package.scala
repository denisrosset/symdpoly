package net.alasc.symdpoly

import net.alasc.perms.Perm
import net.alasc.symdpoly.math.{GenPerm, Phase, PhasedInt}
import net.alasc.symdpoly.sdp.Program

package object matlab {

  implicit class MatlabPerm(val perm: Perm) extends AnyVal {
    def matlabImage(n: Int): Vect = Vect.row(Array.tabulate(n)(i => (perm.image(i) + 1).toDouble))
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
