package net.alasc.symdpoly

import net.alasc.symdpoly.sdp.Program
import solvers.MosekFormat

/** Native interface to Mosek. Add `lib` directory in the modules/mosek folder containing `mosek.jar` to compile this module. */
package object mosek {

  implicit class MosekProgram(val program: Program) {
    def nativeMosek: NativeMosekInstance = new NativeMosekInstance(program)
  }

}
