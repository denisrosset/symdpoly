package net.alasc.symdpoly

/** Syntax enrichment for the typeclasses in [[net.alasc.symdpoly.algebra]]. */
package object syntax {

  object phased extends PhasedSyntax
  object printer extends PrinterSyntax

  object all extends AllSyntax

}
