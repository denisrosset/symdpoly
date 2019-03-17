package net.alasc.symdpoly
package matlab

import net.alasc.symdpoly.joptimizer.JOptimizerInstance
import spire.syntax.cfor._
import scalin.immutable.{Mat, Vec}
import net.alasc.symdpoly.sdp.{Block, Program}
import syntax.phased._
import scalin.immutable.csc._
import spire.std.double._
import spire.std.int._

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
  val b = Vect.col(program.obj(1 until program.obj.length)*objFactor)

  def sparseMatrix(nRows: Int, nCols: Int, data: Seq[(Int, Int, Double)]): Mat[Double] =
    Mat.sparse(nRows, nCols)(Vec(data.map(_._1):_*), Vec(data.map(_._2):_*), Vec(data.map(_._3): _*))

  def convertBlock(block: Block): (Matrix, Matrix) = {
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
    (Matrix(matA), Matrix(matC))
  }

  val (matAseq, matCseq) = program.sdpCon.blocks.map(convertBlock).unzip

  import scalin.immutable.dense._
  val blk = CellArray(Mat.rowMajor(program.sdpCon.blocks.size, 2)(
    program.sdpCon.blocks.flatMap(block => Seq(MatlabChar("s"), Scalar(block.size))): _*
  ))

  def data: Struct =
    Struct("At" -> CellArray(Mat.rowMajor(nBlocks, 1)(matAseq: _*)),
      "b" -> b,
      "C" -> CellArray(Mat.rowMajor(nBlocks, 1)(matCseq: _*)),
      "blk" -> blk,
      "objShift" -> Scalar(objShift),
      "objFactor" -> Scalar(objFactor)
    )
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
