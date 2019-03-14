package net.alasc.symdpoly

import solvers.MosekFormat

package object mosek {

  implicit def mosekInstance(format: MosekFormat): MosekInstance = new MosekInstance(format)

}
