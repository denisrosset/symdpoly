package net.alasc.symdpoly
package free

import shapeless.Witness

import net.alasc.symdpoly.math.{GenPerm, PhasedInt}

class Generator[F <: free.MonoidDef with Singleton:Witness.Aux] protected[symdpoly](val name: String, val opAction: GenPerm) {
  override def toString: String = name + ": " + Generator.prettyPrintGenPerm(opAction, valueOf[F])
}

object Generator {

  def prettyPrintGenPerm(opAction: GenPerm, F: MonoidDef): String = {
    val elements = for {
      i <- 0 until F.nOperators
      PhasedInt(phase, image) = opAction.image(PhasedInt(Phase.one, i)) if phase != Phase.one || i != image
      op = F.opFromIndex(i)
      opImage = F.PhasedOp(phase, F.opFromIndex(image))
    } yield s"$op -> $opImage"
    elements.mkString("{", ", ", "}")
  }

}
