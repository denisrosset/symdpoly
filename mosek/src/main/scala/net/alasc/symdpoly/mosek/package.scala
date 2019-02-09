package net.alasc.symdpoly

import net.alasc.symdpoly.solvers.{MosekInstance, MosekInstance2}

package object mosek {

  implicit def RichMosekInstance(instance: MosekInstance): RichMosekInstance = new RichMosekInstance(instance)
  implicit def RichMosekInstance2(instance: MosekInstance2): RichMosekInstance2 = new RichMosekInstance2(instance)

}
