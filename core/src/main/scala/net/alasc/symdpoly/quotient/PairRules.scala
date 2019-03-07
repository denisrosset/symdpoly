package net.alasc.symdpoly
package quotient

import shapeless.Witness
import spire.syntax.cfor._

import net.alasc.symdpoly
import net.alasc.symdpoly.generic.FreeBasedMono

/** Not used at the moment. Fast storage of rules applying to pairs of operators. */
class PairRules[F <: free.MonoidDef.Aux[F] with Singleton](val width: Int, val lookup: Array[Int], val custom: Map[(F#Op, F#Op), FreeBasedMono[F, F]])(implicit wF: Witness.Aux[F]) {
  import PairRules.{cellIndex, getMaskIndex}
  // 4 bits per value, so each Int in the array stores 8 rules
  def F: F = wF.value
  def rule(i: Int, j: Int): Int = getMaskIndex(j, lookup(cellIndex(i, j, width)))
  def applyRules(op1: F#Op, op2: F#Op): FreeBasedMono[F, F] = rule(F.indexFromOp(op1), F.indexFromOp(op2)) match {
    case PairRules.SetToZero => symdpoly.generic.FreeBasedMono.zero[F, F]
    case PairRules.Preserve => symdpoly.generic.FreeBasedMono(op1, op2)
    case PairRules.RemoveBoth => symdpoly.generic.FreeBasedMono.one[F, F]
    case PairRules.Swap => symdpoly.generic.FreeBasedMono(op2, op1)
    case PairRules.KeepFirst => symdpoly.generic.FreeBasedMono(op1)
    case PairRules.Custom => custom((op1, op2))
  }
}

object PairRules {
  val SetToZero = 0
  val Preserve = 1
  val RemoveBoth = 2
  val Swap = 3
  val KeepFirst = 4
  val Custom = 5
  @inline def cellIndex(i: Int, j: Int, width: Int): Int = {
    val jcoarse = j >>> 3
    // val jfine = j & 0x7
    i * (width >>> 3) + jcoarse
  }
  @inline def getMaskIndex(j: Int, cellValue: Int): Int = {
    (cellValue >>> ((j & 0x7) * 4)) & 0xF
  }
  @inline def setMaskIndex(j: Int, cellValue: Int, rule: Int): Int = {
    cellValue | (rule << ((j & 0x7) * 4))
  }
  def apply[F <: free.MonoidDef.Aux[F] with Singleton](f: PairSubstitutions[F])(implicit wF: Witness.Aux[F]): PairRules[F] = {
    def F: F = wF.value
    val n = F.nOperators
    val width = ((n + 7) >>> 3) << 3
    val lookup = new Array[Int]((width >>> 3) * n)
    val custom = Map.newBuilder[(F#Op, F#Op), FreeBasedMono[F, F]]
    cforRange(0 until n) { i0 =>
      cforRange(0 until n) { i1 =>
        val op0 = F.opFromIndex(i0)
        val op1 = F.opFromIndex(i1)
        val rule: Int = f(op0, op1) match {
          case gw: FreeBasedMono[F, F] if gw.data.isZero => SetToZero
          case gw: FreeBasedMono[F, F] =>
            assert(gw.data.length <= 2, "A substitution rule cannot increase the length of a word")
            if (!gw.data.phase.isOne) {
              custom += ((op0, op1) -> gw)
            }
            gw.data.length match {
              case 0 => RemoveBoth
              case 1 =>
                if (gw.data.indices(0) == i0) KeepFirst else {
                  custom += ((op0, op1) -> gw)
                  Custom
                }
              case 2 =>
                if (gw.data.indices(0) == i0 && gw.data.indices(1) == i1) Preserve
                else if (gw.data.indices(0) == i1 && gw.data.indices(1) == i0) Swap
                else {
                  custom += ((op0, op1) -> gw)
                  Custom
                }
            }
        }
        val ci = cellIndex(i0, i1, width)
        lookup(ci) = setMaskIndex(i1, lookup(ci), rule)
      }
    }
    new PairRules[F](width, lookup, custom.result())
  }
}
