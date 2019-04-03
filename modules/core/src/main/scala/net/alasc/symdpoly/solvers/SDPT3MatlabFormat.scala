package net.alasc.symdpoly
package solvers

import matlab._
import sdp.{Block, Program}
import scalin.immutable.csc._
import scalin.immutable.{Mat, Vec}
import spire.std.double._

import net.alasc.symdpoly.math.DoubleCOOMat
import us.hebi.matlab.mat.format.Mat5
import us.hebi.matlab.mat.types.MatFile

/** Export interface for the SDPT3 solver (same format is used in the SDPNAL family)
  *
  * For the data format, see http://www.math.nus.edu.sg/~mattohkc/sdpt3/guide4-0-draft.pdf
  *
  * We write the problem in the dual form
  * {{{
  * maximize b' y
  *
  * A y + z = c
  * z in K
  *
  * => c - A in K
  * }}}
  *
  */
class SDPT3MatlabFormat(val program: Program) extends MatlabFormat {
  require(program.sdpCon.blocks.size == 1)
  val m: Int = program.obj.length - 1 // number of dual variables
  val nBlocks: Int = program.sdpCon.blocks.size

  val objShift = program.obj(0) // constant in objective not supported

  val objFactor = if (program.direction == Direction.Maximize) 1.0 else -1.0
  val b = (program.obj(1 until program.obj.length)*objFactor).toMatlabCol

  def sparseMatrix(nRows: Int, nCols: Int, data: Seq[(Int, Int, Double)]): DoubleCOOMat =
    DoubleCOOMat(nRows, nCols, data.map(_._1).toArray, data.map(_._2).toArray, data.map(_._3).toArray)

  def convertBlock(block: Block): (DoubleCOOMat, DoubleCOOMat) = {
    val d: Int = block.size
    val n: Int = d * (d + 1) / 2
    val sqrt2 = spire.math.sqrt(2.0)
    // the columns of A represent the SDP constraint, the rows the variables
    val matA = sparseMatrix(n, m, for {
      i <- 0 until block.nEntries
      j = block.basisIndices(i) if j > 0
      r = block.rowIndices(i) // upper triangle
      c = block.colIndices(i) if c >= r
      e = if (r == c) block.coefficients(i) else block.coefficients(i) * sqrt2
    } yield (r + c * (c + 1)/2, j - 1, -e))
    val matC = sparseMatrix(d, d, for {
      i <- 0 until block.nEntries
      j = block.basisIndices(i) if j == 0
      r = block.rowIndices(i)
      c = block.colIndices(i)
      e = if (r == c) block.coefficients(i) else block.coefficients(i) * sqrt2
    } yield (r, c, e))
    (matA, matC)
  }

  val (matAseq, matCseq) = program.sdpCon.blocks.map(convertBlock).unzip

  import scalin.immutable.dense._
  val blk = Mat.rowMajor(program.sdpCon.blocks.size, 2)(
    program.sdpCon.blocks.flatMap(block => Seq(Mat5.newString("s"), Mat5.newScalar(block.size))): _*
  ).toMatlab

  def data: MatFile =
    Mat5.newMatFile().addArray("At", Mat.rowMajor(nBlocks, 1)(matAseq.map(_.toMatlab): _*).toMatlab)
    .addArray("b", b)
    .addArray("C", Mat.rowMajor(nBlocks, 1)(matCseq.map(_.toMatlab): _*).toMatlab)
    .addArray("blk", blk)
    .addArray("objShift", Mat5.newScalar(objShift))
    .addArray("objFactor", Mat5.newScalar(objFactor))

}

object SDPT3MatlabFormat {

  //  TODO: we do not support yet multiple blocks, inequality or equality constraints
  //        with the proper SDPT3 encoding.
  def apply(program: Program): SDPT3MatlabFormat =
    if (program.eqA.nRows > 0) SDPT3MatlabFormat(program.convertEqualitiesToInequalities)
    else if (program.ineqA.nRows > 0) SDPT3MatlabFormat(program.convertInequalitiesToBlock)
    else if (program.sdpCon.blocks.size > 1) SDPT3MatlabFormat(program.mergeBlocks)
    else new SDPT3MatlabFormat(program)

}
