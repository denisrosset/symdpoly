package net.alasc.symdpoly
package generic

import cats.evidence.{As, Is}
import shapeless.Witness
import spire.algebra.{Involution, Order}

import net.alasc.symdpoly.algebra.MultiplicativeBinoid

abstract class Mono[M <: generic.MonoidDef with Singleton:Witness.Aux] { self: M#Monomial =>

}
