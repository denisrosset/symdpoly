package net.alasc.symdpoly
package matlab

import spire.math.Complex
import spire.syntax.cfor._

import com.jmatio.io.MatFileWriter
import com.jmatio.types._
import cyclo.Cyclo

import net.alasc.finite.FaithfulPermutationActionBuilder
import net.alasc.perms.Perm
import net.alasc.symdpoly.math.GenPerm
import net.alasc.syntax.all._
import scalin.immutable.{Mat, Vec}
import scalin.immutable.csc._
import spire.std.double._
import spire.std.int._

import net.alasc.algebra.PermutationAction
import syntax.phased._
import scalin.syntax.all._

import net.alasc.symdpoly.sdp.{Block, Program, RepMat}


/** Export interface for the Sedumi solver
  *
  * We write the problem as
  *
  * maximize b * y
  *
  * A' y + s = c
  * s in K*
  *
  * => c - A' y in K*
  *
  */
case class SedumiMatlabFormat(val program: Program) extends MatlabFormat {

  def convertBlock(block: Block): (Mat[Double], Vec[Double]) = {
    import scalin.immutable.csc._
    val n = block.size

    def index(r: Int, c: Int): Int = c * n + r // col major storage
    val datab = for {
      i <- 0 until block.nEntries
      j = block.basisIndices(i) if j == 0
      r = block.rowIndices(i)
      c = block.colIndices(i)
      e = block.coefficients(i)
    } yield (index(r, c), e)
    val dataA = for {
      i <- 0 until block.nEntries
      j = block.basisIndices(i) if j > 0
      r = block.rowIndices(i)
      c = block.colIndices(i)
      e = block.coefficients(i)
    } yield (j - 1, index(r, c), -e) // note the transpose here, basis index is the row index
    // and there is a sign change
    val matA = Mat.sparse[Double](n * n, block.basisSize - 1)(Vec(dataA.map(_._1): _*), Vec(dataA.map(_._2): _*), Vec(dataA.map(_._3): _*))
    val vecA = Vec.fromMutable(n * n, 0.0) { mut =>
      for ((i, e) <- datab) mut(i) := e
    }
    (matA, vecA)
  }

  def data: Struct = {
    val k = Struct(
      "f" -> Scalar(program.eqA.nRows), "l" -> Scalar(program.ineqA.nRows),
      "q" -> Vect.emptyRow, "r" -> Vect.emptyRow,
      "s" -> Vect.row(program.sdpCon.blocks.map(_.size.toDouble)))
    val eqb = program.eqA(::, 0)
    val ineqb = program.ineqA(::, 0)
    val eqA = -program.eqA(::, 1 until program.eqA.nCols).t
    val ineqA = -program.ineqA(::, 1 until program.ineqA.nCols).t
    val (blocksA, blocksb) = program.sdpCon.blocks.map(convertBlock).unzip
    val matA = Matrix(Seq[Mat[Double]](Seq(eqA, ineqA) ++ blocksA: _*).reduce(_ horzcat _))
    val vecb = Vect.col(Seq[Vec[Double]](Seq(eqb, ineqb) ++ blocksb: _*).reduce(_ cat _))
    val sign = program.direction match {
      case Direction.Minimize => -1.0
      case Direction.Maximize => 1.0
    }
    val c = Vect.col(program.obj(1 until program.obj.length) * sign)
    if (program.sdpCon.symmetryGroup.isTrivial)
      Struct("K" -> k, "A" -> matA, "b" -> vecb, "c" -> c, "objShift" -> Scalar(program.obj(0)), "objFactor" -> Scalar(sign))
    else {
      val permSize = program.sdpCon.symmetryGroup.largestMovedPoint.getOrElse(-1) + 1
      // we use the right action convention, while the Sedumi extended file format uses left action, so we need to invert the permutations,
      // but not the matrices (the matrix multiplication convention is stable)
      val nGenerators = program.sdpCon.symmetryGroup.nGenerators
      val permGenerators = program.sdpCon.symmetryGroup.generators.map(_.inverse.matlabImage(permSize))
      def generatorImages: Seq[Mat[Double]] = program.sdpCon.symmetryGroup.generators.map(p => program.sdpCon.representation(p).toMat)
      import scalin.immutable.dense._
      val g = CellArray(Mat.rowMajor(1, nGenerators)(permGenerators: _*))
      val rho = CellArray(Mat.rowMajor(1, nGenerators)(generatorImages.map(Matrix):_*))
      Struct("K" -> k, "A" -> matA, "b" -> vecb, "c" -> c, "objShift" -> Scalar(program.obj(0)), "objFactor" -> Scalar(sign), "G" -> g, "rho" -> rho)
    }
  }

}
