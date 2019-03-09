package net.alasc.symdpoly

import shapeless.Witness

package object symmetries {

  def symmetrize[
    E <: generic.Evaluator[M] with Singleton: Witness.Aux,
    M <: generic.MonoidDef with Singleton: Witness.Aux,
  ](relaxation: Relaxation[E, M]): Relaxation[_, M] = ???

}
