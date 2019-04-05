package net.alasc.symdpoly
package solvers

import matlab._
import net.alasc.symdpoly.sdp.{Block, Program}
import scalin.immutable.{Mat, Vec}
import scalin.immutable.csc._
import spire.std.double._
import spire.std.int._

import scalin.syntax.all._
import us.hebi.matlab.mat.format.Mat5

import net.alasc.symdpoly.math.DoubleCOOMat
import us.hebi.matlab.mat.types.MatFile

/** Export interface for the SCS solver
  *
  * Data provided in the format of https://github.com/bodono/scs-matlab, see also
  * https://github.com/cvxgrp/scs for the internal details.
  *
  * We write the problem as
  * {{{
  * minimize c'x
  * Ax + s = b
  * s in K
  *
  * => b - Ax in K
  * }}}
  * but our problem formulation is written as Ax + b in K, so a sign change is required
  * for the matrix A.
  *
  */
case class SCSMatlabFormat(val sdp: Program) extends MatlabFormat {

  def convertBlock(block: Block): (DoubleCOOMat, Vec[Double]) = {
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
    val matA = DoubleCOOMat(compactSize, block.basisSize - 1, dataA.map(_._1).toArray, dataA.map(_._2).toArray, dataA.map(_._3).toArray)
    val vecA = Vec.fromMutable(compactSize, 0.0) { mut =>
      for ( (i, e) <- datab ) mut(i) := e
    }
    (matA, vecA)
  }

  def data: MatFile = {
    val k = Mat5.newStruct()
      .set("f", Mat5.newScalar(sdp.eqA.nRows))
      .set("l", Mat5.newScalar(sdp.ineqA.nRows))
      .set("q", Mat5.newScalar((0)))
      .set("s", Vec(sdp.sdpCon.blocks.map(_.size.toDouble): _*).toMatlabRow)
      .set("ep", Mat5.newScalar(0))
      .set("ed", Mat5.newScalar(0))
      .set("p", Vec[Double]().toMatlabRow)

    // sdp.eqA * [1; y] = 0
    // sdp.ineqA * [1; y] >= 0
    import scalin.immutable.csc._
    val eqb = sdp.eqA(::, 0)
    val ineqb = sdp.ineqA(::, 0)
    val eqA = -sdp.eqA(::, 1 until sdp.eqA.nCols)
    val ineqA = -sdp.ineqA(::, 1 until sdp.ineqA.nCols)
    val (blocksA, blocksb) = sdp.sdpCon.blocks.map(convertBlock).unzip
    val matA = if (eqA.nRows == 0 && ineqA.nRows == 0) DoubleCOOMat.vertcat(sdp.nY - 1, blocksA: _*).toMatlab
    else (blocksA.foldLeft(eqA vertcat ineqA) { case (prev, mat) => prev vertcat mat }).toMatlab
    val vecb = (blocksb.foldLeft(eqb cat ineqb) { case (prev, vec) => prev cat vec }).toMatlabCol
    val sign = sdp.direction match {
      case Direction.Minimize => 1.0
      case Direction.Maximize => -1.0
    }
    val c = (sdp.obj(1 until sdp.obj.length) * sign).toMatlabCol
    Mat5.newMatFile()
        .addArray("cones", k)
        .addArray("data", Mat5.newStruct().set("A", matA).set("b", vecb).set("c", c))
        .addArray("objShift", Mat5.newScalar(sdp.obj(0)))
        .addArray("objFactor", Mat5.newScalar(sign))
  }

}
