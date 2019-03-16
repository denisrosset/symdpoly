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

import net.alasc.symdpoly.sdp.{Block, Program}

/** Export interface for the SCS solver
  *
  * Data provided in the format of https://github.com/bodono/scs-matlab, see also
  * https://github.com/cvxgrp/scs for the internal details.
  *
  * We write the problem as
  *
  * minimize c'x
  * Ax + s = b
  * s in K
  *
  * => b - Ax in K
  *
  * but our problem formulation is written as Ax + b in K, so a sign change is required
  * for the matrix A.
  *
  */
case class SCSMatlabFormat(val sdp: Program) extends MatlabFormat {

  def convertBlock(block: Block): (Mat[Double], Vec[Double]) = {
    import scalin.immutable.csc._
    val n = block.size
    val compactSize = (n + 1)*n/2
    def index(r: Int, c: Int): Int = (n*(n+1)/2) - (n-c+1)*(n-c)/2 + r - c
    val sqrt2 = spire.math.sqrt(2.0)
    val datab = for {
      i <- 0 until block.nEntries
      j = block.basisIndices(i) if j == 0
      r = block.rowIndices(i)
      c = block.colIndices(i) if r >= c // only the lower triangle
      e = if (r == c) block.coefficients(i) else block.coefficients(i) * sqrt2
    } yield (index(r, c), e)
    val dataA = for {
      i <- 0 until block.nEntries
      j = block.basisIndices(i) if j > 0
      r = block.rowIndices(i)
      c = block.colIndices(i) if r >= c // only the lower triangle
      e = if (r == c) block.coefficients(i) else block.coefficients(i) * sqrt2
    } yield (index(r, c), j - 1, -e) // sign change here
    // lower triangle
    val matA = Mat.sparse[Double](compactSize, block.basisSize - 1)(Vec(dataA.map(_._1): _*), Vec(dataA.map(_._2): _*), Vec(dataA.map(_._3): _*))
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
        "s" -> Vect.row(sdp.sdpCon.blocks.map(_.size.toDouble).toArray),
        "ep" -> Scalar(0),
        "ed" -> Scalar(0),
        "p" -> Vect.emptyRow
      )
    // sdp.eqA * [1; y] = 0
    // sdp.ineqA * [1; y] >= 0
    import scalin.immutable.csc._
    val eqb = sdp.eqA(::, 0)
    val ineqb = sdp.ineqA(::, 0)
    val eqA = -sdp.eqA(::, 1 until sdp.eqA.nCols)
    val ineqA = -sdp.ineqA(::, 1 until sdp.ineqA.nCols)
    val (blocksA, blocksb) = sdp.sdpCon.blocks.map(convertBlock).unzip
    val matA = Matrix(blocksA.foldLeft(eqA vertcat ineqA) { case (prev, mat) => prev vertcat mat })
    val vecb = Vect.col(blocksb.foldLeft(eqb cat ineqb) { case (prev, vec) => prev cat vec })
    val sign = sdp.direction match {
      case Direction.Minimize => 1.0
      case Direction.Maximize => -1.0
    }
    val c = Vect.col(sdp.obj(1 until sdp.obj.length) * sign)
    Struct("cones" -> k, "data" -> Struct("A" -> matA, "b" -> vecb, "c" -> c), "objShift" -> Scalar(sdp.obj(0)), "objFactor" -> Scalar(sign))
  }

}
