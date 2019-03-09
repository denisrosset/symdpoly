package net.alasc.symdpoly
package generic

import shapeless.Witness

abstract class Mono[M <: generic.MonoidDef with Singleton:Witness.Aux] { self: M#Monomial =>

}
