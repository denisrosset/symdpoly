package net.alasc.symdpoly

import net.alasc.symdpoly.solvers.MosekInstance

package object mosek {

  implicit def RichMosekInstance(instance: MosekInstance): RichMosekInstance = new RichMosekInstance(instance)

}
