package net.alasc.symdpoly
package matlab

import java.util.{ArrayList => JavaArrayList, Collection => JavaCollection}

import spire.syntax.cfor._

import scalin.immutable.{Mat, Vec}

import com.jmatio.io.MatFileWriter
import com.jmatio.types.{MLArray, MLCell, MLChar, MLDouble, MLSparse, MLStructure}
import syntax.phased._
import spire.std.double._
import spire.std.int._
import scalin.syntax.all._

/** Data provided in the format of https://github.com/bodono/scs-matlab */
class SCSMatlabFormat(val sdp: SDP) extends MatlabFormat {

  def convertBlock(block: SDP.Block): (Mat[Double], Vec[Double]) = {
    import scalin.immutable.csc._
    val n = block.size
    val compactSize = (n + 1)*n/2
    def index(r: Int, c: Int): Int = (n*(n+1)/2) - (n-c+1)*(n-c)/2 + r - c
    val datab = for {
      i <- 0 until block.nEntries
      j = block.basisIndex(i) if j == 0
      r = block.rowIndex(i)
      c = block.colIndex(i) if r >= c // only the lower triangle
      e = block.coeffs(i)
    } yield (index(r, c), e)
    val dataA = for {
      i <- 0 until block.nEntries
      j = block.basisIndex(i) if j > 0
      r = block.rowIndex(i)
      c = block.colIndex(i) if r >= c // only the lower triangle
      e = block.coeffs(i)
    } yield (index(r, c), j - 1, e)
    // lower triangle
    val matA = Mat.sparse[Double](compactSize, block.basisSize)(Vec(dataA.map(_._1): _*), Vec(dataA.map(_._2): _*), Vec(dataA.map(_._3): _*))
    val vecA = Vec.fromMutable(compactSize, 0.0) { mut =>
      for ( (i, e) <- datab ) mut(i) := e
    }
    (matA, vecA)
  }

  def data: Struct = {
    val k = Struct(
        "f" -> Scalar(sdp.eqA.nRows),
        "l" -> Scalar(sdp.ineqA.nRows),
        "q" -> Scalar(0),
        "s" -> Vect.row(sdp.blocks.map(_.size.toDouble).toArray),
        "ep" -> Scalar(0),
        "ed" -> Scalar(0),
        "p" -> Vect.emptyRow
      )
    // sdp.eqA * [1; y] = 0
    // sdp.ineqA * [1; y] >= 0
    import scalin.immutable.csc._
    val eqb = -sdp.eqA(::, 0)
    val ineqb = -sdp.ineqA(::, 0)
    val eqA = sdp.eqA(::, 1 until sdp.eqA.nCols)
    val ineqA = sdp.ineqA(::, 1 until sdp.ineqA.nCols)
    val (blocksA, blocksb) = sdp.blocks.map(convertBlock).unzip
    val matA = Matrix(blocksA.foldLeft(eqA vertcat ineqA) { case (prev, mat) => prev vertcat mat })
    val vecb = Vect.col(blocksb.foldLeft(eqb cat ineqb) { case (prev, vec) => prev cat vec })
    val c = Vect.col(sdp.direction match {
      case Direction.Minimize => sdp.obj(1 until sdp.obj.length)
      case Direction.Maximize => -sdp.obj(1 until sdp.obj.length)
    })
    Struct("cones" -> k, "data" -> Struct("A" -> matA, "b" -> vecb, "c" -> c), "objShift" -> Scalar(sdp.obj(0)))
  }

}
