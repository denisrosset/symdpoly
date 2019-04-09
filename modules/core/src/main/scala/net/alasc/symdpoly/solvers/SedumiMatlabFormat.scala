package net.alasc.symdpoly
package solvers

import net.alasc.symdpoly.solvers.matlab._
import net.alasc.symdpoly.sdp.{Block, Program}
import scalin.immutable.{Mat, Vec}
import scalin.immutable.csc._
import spire.std.double._
import spire.std.int._
import scalin.syntax.all._

import net.alasc.symdpoly.math.DoubleCOOMat
import us.hebi.matlab.mat.format.Mat5
import us.hebi.matlab.mat.types.MatFile

/** Export interface for the Sedumi solver
  *
  * We write the problem as
  * {{{
  * maximize b * y
  *
  * A' y + s = c
  * s in K*
  *
  * => c - A' y in K*
  * }}}
  */
case class SedumiMatlabFormat(val program: Program) extends MatlabFormat {

  def convertBlock(block: Block): (DoubleCOOMat, Vec[Double]) = {
    val n = block.size
    def index(r: Int, c: Int): Int = c * n + r // col major storage
    val datac = for {
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
    val matA = DoubleCOOMat(block.basisSize - 1, n * n, dataA.map(_._1).toArray, dataA.map(_._2).toArray, dataA.map(_._3).toArray)
    val vecc = Vec.fromMutable(n * n, 0.0) { mut =>
      for ((i, e) <- datac) mut(i) := e
    }
    (matA, vecc)
  }

  def data: MatFile = {
    logNormal("Preparing SeDuMi file")
    val k = Mat5.newStruct()
      .set("f", Mat5.newScalar(program.eqA.nRows))
      .set("l", Mat5.newScalar(program.ineqA.nRows))
      .set("q", Mat.zeros[Double](1, 0).toMatlab)
      .set("r",  Mat.zeros[Double](1, 0).toMatlab)
      .set("s", Vec(program.sdpCon.blocks.map(_.size.toDouble): _*).toMatlabRow)
    logVerbose(s"Number of variables in the dual form: ${program.ineqA.nCols}")
    logVerbose(s"Equality constraints in the dual form: ${program.eqA.nRows}")
    logVerbose(s"Inequality constraints in the dual form: ${program.ineqA.nRows}")
    logVerbose(s"SDP block sizes: ${program.sdpCon.blocks.map(_.size).toList}")
    val eqc = program.eqA(::, 0)
    val ineqc = program.ineqA(::, 0)
    val eqA = -program.eqA(::, 1 until program.eqA.nCols).t
    val ineqA = -program.ineqA(::, 1 until program.ineqA.nCols).t
    logVerbose(s"Transforming blocks")
    val (blocksA, blocksb) = program.sdpCon.blocks.map(convertBlock).unzip
    val matA = if (eqA.nCols == 0 && ineqA.nCols == 0) DoubleCOOMat.horzcat(program.nY - 1, blocksA: _*).toMatlab else
      Seq[Mat[Double]](Seq(eqA, ineqA) ++ blocksA: _*).reduce(_ horzcat _).toMatlab
    val vecc = Seq[Vec[Double]](Seq(eqc, ineqc) ++ blocksb: _*).reduce(_ cat _).toMatlabCol
    val sign = program.direction match {
      case Direction.Minimize => -1.0
      case Direction.Maximize => 1.0
    }
    val vecb = (program.obj(1 until program.obj.length) * sign).toMatlabCol
    if (program.sdpCon.symmetryGroup.isTrivial)
      Mat5.newMatFile()
        .addArray("K", k)
        .addArray("A", matA)
        .addArray("b", vecb)
        .addArray("c", vecc)
        .addArray("objShift", Mat5.newScalar(program.obj(0)))
        .addArray("objFactor", Mat5.newScalar(sign))
    else {
      val permSize = program.sdpCon.symmetryGroup.largestMovedPoint.getOrElse(-1) + 1
      // we use the right action convention, while the Sedumi extended file format uses left action, so we need to invert the permutations,
      // but not the matrices (the matrix multiplication convention is stable)
      val nGenerators = program.sdpCon.symmetryGroup.nGenerators
      logVerbose(s"Writing group representation of a permutation group of domain size ${permSize} with ${nGenerators} generator(s)")
      val permGenerators = program.sdpCon.symmetryGroup.generators.map(_.inverse.matlabImage(permSize))
      def generatorImages: Seq[Mat[Double]] = program.sdpCon.symmetryGroup.generators.map(p => program.sdpCon.representation(p).toMat)
      import scalin.immutable.dense._
      val g = permGenerators.toMatlabRow
      val rho = generatorImages.map(_.toMatlab).toMatlabRow
      Mat5.newMatFile()
        .addArray("K", k)
        .addArray("A", matA)
        .addArray("b", vecb)
        .addArray("c", vecc)
        .addArray("objShift", Mat5.newScalar(program.obj(0)))
        .addArray("objFactor", Mat5.newScalar(sign))
        .addArray("G", g)
        .addArray("rho", rho)
    }
  }

}
