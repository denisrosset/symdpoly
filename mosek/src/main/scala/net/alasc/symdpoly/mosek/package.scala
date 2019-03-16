package net.alasc.symdpoly

import net.alasc.symdpoly.sdp.Program
import solvers.MosekFormat

package object mosek {

  implicit class MosekProgram(val program: Program) {
    def nativeMosek: NativeMosekInstance = new NativeMosekInstance(program)
  }

}
