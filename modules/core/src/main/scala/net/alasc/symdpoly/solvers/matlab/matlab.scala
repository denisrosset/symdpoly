package net.alasc.symdpoly.solvers

import scalin.immutable.{Mat, Vec}

import net.alasc.perms.Perm
import net.alasc.symdpoly.math.{DoubleCOOMat, DoubleCSCMat, GenPerm, Phase, PhasedInt}
import us.hebi.matlab.mat.format.Mat5
import us.hebi.matlab.mat.types.{Cell, Array => MatlabArray}
import spire.syntax.cfor._

package object matlab {

  implicit class MatlabPerm(val perm: Perm) extends AnyVal {
    import scalin.immutable.dense._
    def matlabImage(n: Int): DoubleMatWrapper = Vec(Seq.tabulate(n)(i => (perm.image(i) + 1).toDouble): _*).toMatlabRow
  }

  implicit class MatlabGenPerm(val genPerm: GenPerm) extends AnyVal {
    /** Computes the signed permutation on 1-based indices, for the domain {-n...-1 1...n} */
    def matlabImage(n: Int): Array[Int] = {
      Array.tabulate[Int](n) { pIndex =>
        genPerm.image(PhasedInt(Phase.one, pIndex)) match {
          case PhasedInt(Phase.one, iIndex) => iIndex + 1
          case PhasedInt(Phase.minusOne, iIndex) => -(iIndex + 1)
        }
      }
    }
  }

  implicit class RichMatDouble(val lhs: Mat[Double]) {
    def toMatlab: DoubleMatWrapper = lhs match {
      case coo: DoubleCOOMat => new DoubleCSCMatWrapper(coo.toDoubleCSCMat)
      case csc: scalin.immutable.CSCMat[Double] =>
        val csc1 = new DoubleCSCMat(csc.nRows, csc.nCols, csc.colPtrs, csc.rowIndices, csc.data.map(_.asInstanceOf[Double]))
        csc1.toMatlab
      case csc: DoubleCSCMat => new DoubleCSCMatWrapper(csc)
      case mat => new DoubleDenseMatWrapper(mat)
    }
  }

  implicit class RichVecDouble(val vec: Vec[Double]) {
    import scalin.immutable.dense._
    def toMatlabRow: DoubleMatWrapper = vec.toRowMat.toMatlab
    def toMatlabCol: DoubleMatWrapper = vec.toColMat.toMatlab
  }

  implicit class RichMatMatlabArray[A <: MatlabArray](val mat: Mat[A]) {
    def toMatlab: Cell = {
      val res = Mat5.newCell(mat.nRows, mat.nCols)
      cforRange(0 until mat.nRows) { r =>
        cforRange(0 until mat.nCols) { c =>
          res.set(r, c, mat(r, c))
        }
      }
      res
    }
  }

  implicit class RichSeqMatlabArray(val seq: Seq[MatlabArray]) {
    def toMatlabRow: Cell = {
      val res = Mat5.newCell(1, seq.length)
      cforRange(0 until seq.length) { i =>
        res.set(i, seq(i))
      }
      res
    }
  }

}
