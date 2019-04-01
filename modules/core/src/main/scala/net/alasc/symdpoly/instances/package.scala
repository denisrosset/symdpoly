package net.alasc.symdpoly

/** Typeclasses instances for libraries used in SymDPoly.
  *
  * Used mostly to provide cats-based structures to libraries not depending on cats. */
package object instances {

  object invariant extends InvariantInstances

  object all extends AllInstances

}
