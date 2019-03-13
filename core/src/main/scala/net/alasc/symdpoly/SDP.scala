package net.alasc.symdpoly

import java.io.{BufferedWriter, FileWriter, StringWriter, Writer}

import spire.syntax.cfor.cforRange

import scalin.immutable.{Mat, Vec}
import spire.std.double._
import scalin.immutable.dense._

/** Description of an semidefinite program extended dual.
  *
  * The conic linear program is given by:
  *
  *   maximize   sum_i objToMaximize(i) * y(i)
  *   over real y(0), ..., y(m-1)
  *
  *   subject to
  *
  *   y(0) == 1
  *   for all j: sum_i y(i) blocks(j).basis(i) >= 0
  *   eqA * y == 0
  *   ineqA * y >= 0 (component-wise)
  */
case class SDP(objToMaximize: Vec[Double], blocks: Seq[SDP.Block], eqA: Mat[Double], ineqA: Mat[Double]) {

  def convertEqualities: SDP = SDP(objToMaximize, blocks, Mat.zeros[Double](0, objToMaximize.length), ineqA vertcat eqA vertcat (-eqA))

  def writeDataSDPA(writer: Writer): Unit =
    if (eqA.nRows > 0) convertEqualities.writeDataSDPA(writer) else {
      val m: Int = objToMaximize.length - 1
      objToMaximize(0) match {
        case 0 =>
        case constantTerm =>
          writer.append(s"* SDPA solves a minimization dual problem, while we express a maximization problem \n")
          writer.append(s"* also, the original objective has constant term cte = ${constantTerm} (constant terms are not supported by SDPA)\n")
          writer.append(s"* the real objective is thus cte - obj_SDPA value\n")
      }
      val nBlocks = blocks.length + (if (ineqA.nRows > 0) 1 else 0)
      writer.append(s"$m\n")
      writer.append(s"$nBlocks\n")
      val sdpaBlockSizes = blocks.map(_.size) ++ (if (ineqA.nRows > 0) Seq(-ineqA.nRows) else Seq.empty)
      writer.append(sdpaBlockSizes.mkString("", " ", "\n"))
      writer.append((1 until objToMaximize.length).map(i => -objToMaximize(i)).mkString("", " ", "\n"))
      cforRange(0 until blocks.length) { b =>
        val block = blocks(b)
        cforRange(0 until block.nEntries) { i =>
          val di = block.basisIndex(i)
          val r = block.rowIndex(i)
          val c = block.colIndex(i)
          val e = if (di == 0) -block.coeffs(i) else block.coeffs(i)
          if (c >= r) // upper triangle
            writer.append(s"$di ${b + 1} ${r + 1} ${c + 1} $e\n")
        }
      }
      if (ineqA.nRows > 0) {
        cforRange(0 until ineqA.nRows) { r =>
          cforRange(0 until ineqA.nCols) { c =>
            if (ineqA(r, c) != 0) {
              val e = if (c == 0) -ineqA(r, c) else ineqA(r, c)
              writer.append(s"$c $nBlocks $r $r $e\n")
            }
          }
        }
      }
    }


  def writeFileSDPA(filename: String): Unit = {
    val file = new java.io.File(filename)
    import resource._
    for {
      fileWriter <- managed(new FileWriter(file))
      bufferedWriter <- managed(new BufferedWriter(fileWriter))
    } {
      writeDataSDPA(bufferedWriter)
    }
  }

  def dataSDPA: String = {
    val sw = new StringWriter
    writeDataSDPA(sw)
    sw.toString
  }
}

object SDP {

  case class Block(size: Int, basisIndex: Array[Int], rowIndex: Array[Int], colIndex: Array[Int], coeffs: Array[Double]) {
    def nEntries: Int = basisIndex.length
  }

}