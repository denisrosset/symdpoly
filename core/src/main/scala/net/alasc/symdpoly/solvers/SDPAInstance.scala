package net.alasc.symdpoly
package solvers

import java.io.{BufferedWriter, FileWriter, PrintWriter, StringWriter, Writer}

import spire.syntax.cfor.cforRange

trait TextInstance {
  def writeData(writer: Writer): Unit
  def writeFile(filename: String): Unit = {
    val file = new java.io.File(filename)
    import resource._
    for {
      fileWriter <- managed(new FileWriter(file))
      bufferedWriter <- managed(new BufferedWriter(fileWriter))
    } {
      writeData(bufferedWriter)
    }
  }

  def data: String = {
    val sw = new StringWriter
    writeData(sw)
    sw.toString
  }
}

case class SDPAInstance(val sdp: SDP) extends TextInstance {
  import sdp._

  val m: Int = objToMaximize.length - 1

  def writeData(writer: Writer): Unit =
    if (eqA.nRows > 0) sdp.convertEqualitiesToInequalities.sdpa.writeData(writer) else {
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

}

class OldSDPAInstance(val relaxation: OldRelaxation[_, _]) extends OldInstance {
  import net.alasc.symdpoly.solvers.UpperTriangular.SparseMatrix
  import relaxation.{momentMatrix, objectiveVector}
  import momentMatrix.matrixSize

  val m: Int = momentMatrix.nUniqueMonomials - 1 // number of dual variables
  val nBlocks: Int = 1
  val blocks: Array[Int] = Array(matrixSize)

  val a = Vector.tabulate(m)(i => SparseMatrix.forMoment(momentMatrix, i + 1, -1.0))
  val b = Array.tabulate(m)(i => realCycloToDouble(objectiveVector(i + 1)))
  val objConstant = realCycloToDouble(objectiveVector(0))
  val C = SparseMatrix.forMoment(momentMatrix, 0, -1.0)

  def writeData(writer: Writer): Unit = {
    writer.append(s"* SDPA solves a minimization dual problem, while we express a maximization problem \n")
    writer.append(s"* also, the original objective has constant term cte = ${objConstant} (constant terms are not supported by SDPA)\n")
    writer.append(s"* the real objective is thus cte - obj_SDPA value\n")
    writer.append(s"$m\n")
    writer.append(s"$nBlocks\n")
    writer.append(s"${blocks.mkString(" ")}\n")
    writer.append(s"${b.mkString(" ")}\n")
    cforRange(0 until C.nEntries) { j =>
      writer.append(s"0 1 ${C.rows(j)+1} ${C.cols(j)+1} ${C.data(j)}\n")
    }
    cforRange(0 until m) { i =>
      val block = a(i)
      cforRange(0 until block.nEntries) { j =>
        writer.append(s"${i+1} 1 ${block.rows(j)+1} ${block.cols(j)+1} ${block.data(j)}\n")
      }
    }
  }

  def data: String = {
    val sw = new StringWriter
    writeData(sw)
    sw.toString
  }

  def writeFile(fileName: String): Unit = {
    val file = new java.io.File(fileName)
    import resource._
    for {
      fileWriter <- managed(new FileWriter(file))
      bufferedWriter <- managed(new BufferedWriter(fileWriter))
    } {
      writeData(bufferedWriter)
    }
  }

}
