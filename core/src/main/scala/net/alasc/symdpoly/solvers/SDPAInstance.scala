package net.alasc.symdpoly
package solvers

import java.io.{BufferedWriter, FileWriter, PrintWriter, StringWriter, Writer}

import spire.syntax.cfor.cforRange

class SDPAInstance(val relaxation: Relaxation[_, _]) extends Instance {
  import net.alasc.symdpoly.solvers.UpperTriangular.SparseMatrix
  import relaxation.{gramMatrix, objectiveVector}
  import gramMatrix.matrixSize

  val m: Int = gramMatrix.nUniqueMonomials - 1 // number of dual variables
  val nBlocks: Int = 1
  val blocks: Array[Int] = Array(matrixSize)

  val a = Vector.tabulate(m)(i => SparseMatrix.forMoment(gramMatrix, i + 1, -1.0))
  val b = Array.tabulate(m)(i => cycloToDouble(objectiveVector(i + 1)))
  val objConstant = cycloToDouble(objectiveVector(0))
  val C = SparseMatrix.forMoment(gramMatrix, 0, -1.0)

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

