package net.alasc.symdpoly
package solvers

import java.io.{BufferedWriter, FileWriter, PrintWriter, StringWriter, Writer}

import scalin.Sparse
import scalin.immutable.{Mat, Vec}
import spire.syntax.cfor._

import scalin.syntax.all._

class MosekInstance(val relaxation: Relaxation[_, _]) extends Instance {

  import LowerTriangular.SparseMatrix

  import relaxation.{gramMatrix, objectiveVector}
  import gramMatrix.matrixSize

  val m = gramMatrix.nUniqueMonomials - 1 // number of constraints in the primal / variables in the dual
  val numcon = m
  val numbarvar = 1 // number of semidefinite variables in the primal / LMI in the dual
  val d = matrixSize
  val dimbarvar = Array(d) // dimension of semidefinite cone
  val lenbarvar = Array(d * (d + 1) / 2) // number of scalar semidefinite variables
  val cfix = realCycloToDouble(objectiveVector(0))

  // vector b

  //val bkc = Array.fill(m)(mosek.boundkey.fx)
  val blc = Array.tabulate(m)(i => realCycloToDouble(objectiveVector(i + 1)))
  val buc = blc

  // LMI constant
  val c = SparseMatrix.forMoment(gramMatrix, 0)

  // LMI matrices
  val a = Vector.tabulate(m)(i => SparseMatrix.forMoment(gramMatrix, i + 1, -1.0))

  def writeSparseMatrix(writer: Writer, matrix: SparseMatrix): Unit = {
    writer.append(s"${matrix.nTerms}\n")
    cforRange(0 until matrix.nTerms) { i =>
      writer.append(s"0 ${matrix.rows(i)} ${matrix.cols(i)} ${matrix.data(i)}\n")
    }
  }

  def writeConstraintMatrices(writer: Writer, list: Vector[SparseMatrix]): Unit = {
    val totalTerms = list.map(_.nTerms).foldLeft(0)(_ + _)
    writer.append(s"$totalTerms\n")
    cforRange(0 until list.size) { i =>
      val mat = list(i)
      cforRange(0 until mat.nTerms) { j =>
        writer.append(s"$i 0 ${mat.rows(j)} ${mat.cols(j)} ${mat.data(j)}\n")
      }
    }
  }

  def writeCBFData(writer: Writer): Unit = {
    writer.append("VER\n")
    writer.append("1\n")
    writer.append("\n")
    writer.append("OBJSENSE\n")
    writer.append("MIN\n")
    writer.append("\n")
    writer.append("PSDVAR\n")
    val blocks = Array(d)
    writer.append(s"${blocks.length}\n")
    blocks.foreach { bSize => writer.append(s"$bSize\n") }
    writer.append("\n")
    writer.append("CON\n")
    writer.append(s"$m 1\n")
    writer.append(s"L= $m\n")
    writer.append("\n")
    writer.append("OBJFCOORD\n")
    writeSparseMatrix(writer, c)
    writer.append("\n")
    writer.append("FCOORD\n")
    writeConstraintMatrices(writer, a)
    writer.append("\n")
    writer.append("BCOORD\n")
    writer.append(s"${blc.count(_ != 0)}\n")
    cforRange(0 until blc.length) { i =>
      if (blc(i) != 0.0) {
        writer.append(s"$i ${-blc(i)}\n")
      }
    }
  }

  def writeCBF(fileName: String): Unit = {
    val file = new java.io.File(fileName)
    import resource._
    for {
      fileWriter <- managed(new FileWriter(file))
      bufferedWriter <- managed(new BufferedWriter(fileWriter))
    } {
      writeCBFData(fileWriter)
    }
  }

  def dataCBF: String = {
    val sw = new StringWriter
    writeCBFData(sw)
    sw.toString
  }

}

object MosekInstance {

  def fromLowerTriangularColStacked[F:Sparse](d: Int, vec: Vec[F]): Mat[F] = {
    import scalin.immutable.csc._
    Mat.fromMutable(d, d, Sparse[F].zero) { mat =>
      var i = 0
      cforRange(0 until d) { c =>
        cforRange(c until d) { r =>
          mat(r, c) := vec(i)
          mat(c, r) := vec(i)
          i += 1
        }
      }
    }
  }

}
