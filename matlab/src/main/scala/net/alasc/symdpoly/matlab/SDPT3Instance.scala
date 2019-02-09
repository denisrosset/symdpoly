package net.alasc.symdpoly.matlab

import spire.syntax.cfor._

import com.jmatio.io.MatFileWriter
import com.jmatio.types._
import net.alasc.symdpoly.solvers.{Instance, Instance2}
import net.alasc.symdpoly.{GramMatrix, GramMatrix2, Relaxation, Relaxation2}
import net.alasc.symdpoly.algebra.Phased.syntax._

class SDPT3Instance(val relaxation: Relaxation[_, _, _]) extends Instance {

  import SDPT3Instance.{SparseMatrix, SparseVector}
  import relaxation.{gramMatrix, objectiveVector}
  import gramMatrix.{matrixSize => d}
  // TODO require(gramMatrix.momentSet(0).isOne, "Error: empty/one monomial not part of the relaxation")
  val m: Int = gramMatrix.nUniqueMonomials - 1 // number of dual variables
  val n: Int = d * (d + 1) / 2

  val objShift = cycloToDouble(objectiveVector(0)) // constant in objective not supported

  val b = Array.tabulate(m)(i => cycloToDouble(objectiveVector(i + 1)))

  val C = SparseMatrix.forMoment(gramMatrix, 0, 1.0)

  def aMatrix: SparseMatrix = {
    val columns = Array.tabulate(m)(c => SparseVector.forMoment(gramMatrix, c + 1, -1.0))
    val rows = columns.flatMap(_.indices)
    val cols = columns.zipWithIndex.flatMap { case (col, c) => Array.fill(col.nEntries)(c) }
    val data = columns.flatMap(_.data)
    SparseMatrix(rows, cols, data, n, m)
  }

  val a = aMatrix

  case class Block(kind: String, dim: Int)
  val blocks = Array(Block("s", d))


  def writeFile(fileName: String): Unit = {
    val file = new java.io.File(fileName)
    val mlBlocks = new MLCell("blk", Array(1, 2))
    mlBlocks.set(new MLChar(null, blocks(0).kind), 0, 0)
    mlBlocks.set(new MLDouble(null, Array(blocks(0).dim.toDouble), 1), 0, 1)
    val mlAt = new MLCell("At", Array(1, 1))
    val mlAt0 = new MLSparse("At", Array(n, m), 0, a.nEntries)
    cforRange(0 until a.nEntries) { i =>
      mlAt0.set(a.data(i), a.rows(i), a.cols(i))
    }
    mlAt.set(mlAt0, 0, 0)
    val mlb = new MLDouble("b", b, m)
    val mlC = new MLCell("C", Array(1, 1))
    val mlC0 = new MLSparse("C", Array(d, d), 0, C.nEntries)
    cforRange(0 until C.nEntries) { i =>
      mlC0.set(C.data(i), C.rows(i), C.cols(i))
    }
    mlC.set(mlC0, 0, 0)
    val mlObjShift = new MLDouble("objShift", Array(objShift), 1)
    val list = new java.util.ArrayList[MLArray]()
    list.add(mlAt)
    list.add(mlb)
    list.add(mlC)
    list.add(mlObjShift)
    list.add(mlBlocks)
    new MatFileWriter(file, list)
  }

}

class SDPT3Instance2(val relaxation: Relaxation2[_, _]) extends Instance2 {

  import SDPT3Instance2.{SparseMatrix, SparseVector}
  import relaxation.{gramMatrix, objectiveVector}
  import gramMatrix.{matrixSize => d}
  // TODO require(gramMatrix.momentSet(0).isOne, "Error: empty/one monomial not part of the relaxation")
  val m: Int = gramMatrix.nUniqueMonomials - 1 // number of dual variables
  val n: Int = d * (d + 1) / 2

  val objShift = cycloToDouble(objectiveVector(0)) // constant in objective not supported

  val b = Array.tabulate(m)(i => cycloToDouble(objectiveVector(i + 1)))

  val C = SparseMatrix.forMoment(gramMatrix, 0, 1.0)

  def aMatrix: SparseMatrix = {
    val columns = Array.tabulate(m)(c => SparseVector.forMoment(gramMatrix, c + 1, -1.0))
    val rows = columns.flatMap(_.indices)
    val cols = columns.zipWithIndex.flatMap { case (col, c) => Array.fill(col.nEntries)(c) }
    val data = columns.flatMap(_.data)
    SparseMatrix(rows, cols, data, n, m)
  }

  val a = aMatrix

  case class Block(kind: String, dim: Int)
  val blocks = Array(Block("s", d))


  def writeFile(fileName: String): Unit = {
    val file = new java.io.File(fileName)
    val mlBlocks = new MLCell("blk", Array(1, 2))
    mlBlocks.set(new MLChar(null, blocks(0).kind), 0, 0)
    mlBlocks.set(new MLDouble(null, Array(blocks(0).dim.toDouble), 1), 0, 1)
    val mlAt = new MLCell("At", Array(1, 1))
    val mlAt0 = new MLSparse("At", Array(n, m), 0, a.nEntries)
    cforRange(0 until a.nEntries) { i =>
      mlAt0.set(a.data(i), a.rows(i), a.cols(i))
    }
    mlAt.set(mlAt0, 0, 0)
    val mlb = new MLDouble("b", b, m)
    val mlC = new MLCell("C", Array(1, 1))
    val mlC0 = new MLSparse("C", Array(d, d), 0, C.nEntries)
    cforRange(0 until C.nEntries) { i =>
      mlC0.set(C.data(i), C.rows(i), C.cols(i))
    }
    mlC.set(mlC0, 0, 0)
    val mlObjShift = new MLDouble("objShift", Array(objShift), 1)
    val list = new java.util.ArrayList[MLArray]()
    list.add(mlAt)
    list.add(mlb)
    list.add(mlC)
    list.add(mlObjShift)
    list.add(mlBlocks)
    new MatFileWriter(file, list)
  }

}

object SDPT3Instance2 {

  case class SparseVector(indices: Array[Int], data: Array[Double], length: Int) {
    def nEntries: Int = indices.length
  }

  object SparseVector {
    val sqrt2 = spire.math.sqrt(2.0)
    def forMoment(gramMatrix: GramMatrix2[_, _], momentIndex: Int, factor: Double = 1.0): SparseVector = {
      import gramMatrix.{matrixSize => d}
      val indices = metal.mutable.Buffer.empty[Int]
      val data = metal.mutable.Buffer.empty[Double]
      var index = 0
      cforRange(0 until d) { c =>
        cforRange(0 to c) { r =>
          // matlab has row-major indexing (not that it counts for symmetric matrices...)
          if (gramMatrix.momentIndex(r, c) == momentIndex) {
            indices += index
            if (r == c)
              data += gramMatrix.phase(r, c).toInt.toDouble * factor
            else
              data += gramMatrix.phase(r, c).toInt.toDouble * factor * sqrt2
          }
          index += 1
        }
      }
      SparseVector(indices.toArray, data.toArray, d*(d + 1)/2)
    }
  }

  case class SparseMatrix(rows: Array[Int], cols: Array[Int], data: Array[Double], nRows: Int, nCols: Int) {
    def nEntries: Int = rows.length
    override def toString:String = s"SparseMatrix(${rows.toSeq}, ${cols.toSeq}, ${data.toSeq}, $nRows, $nCols)"
  }

  object SparseMatrix {
    def forMoment(gramMatrix: GramMatrix2[_, _], momentIndex: Int, factor: Double = 1.0): SparseMatrix = {
      import gramMatrix.{matrixSize => d}
      val rows = metal.mutable.Buffer.empty[Int]
      val cols = metal.mutable.Buffer.empty[Int]
      val data = metal.mutable.Buffer.empty[Double]
      cforRange(0 until d) { r =>
        cforRange(0 until d) { c =>
          // matlab has row-major indexing (not that it counts for symmetric matrices...)
          if (gramMatrix.momentIndex(r, c) == momentIndex) {
            rows += r
            cols += c
            data += gramMatrix.phase(r, c).toInt.toDouble * factor
          }
        }
      }
      SparseMatrix(rows.toArray, cols.toArray, data.toArray, d, d)
    }
  }

}
object SDPT3Instance {

  case class SparseVector(indices: Array[Int], data: Array[Double], length: Int) {
    def nEntries: Int = indices.length
  }

  object SparseVector {
    val sqrt2 = spire.math.sqrt(2.0)
    def forMoment(gramMatrix: GramMatrix[_, _], momentIndex: Int, factor: Double = 1.0): SparseVector = {
      import gramMatrix.{matrixSize => d}
      val indices = metal.mutable.Buffer.empty[Int]
      val data = metal.mutable.Buffer.empty[Double]
      var index = 0
      cforRange(0 until d) { c =>
        cforRange(0 to c) { r =>
          // matlab has row-major indexing (not that it counts for symmetric matrices...)
          if (gramMatrix.momentIndex(r, c) == momentIndex) {
            indices += index
            if (r == c)
              data += gramMatrix.phase(r, c).toInt.toDouble * factor
            else
              data += gramMatrix.phase(r, c).toInt.toDouble * factor * sqrt2
          }
          index += 1
        }
      }
      SparseVector(indices.toArray, data.toArray, d*(d + 1)/2)
    }
  }

  case class SparseMatrix(rows: Array[Int], cols: Array[Int], data: Array[Double], nRows: Int, nCols: Int) {
    def nEntries: Int = rows.length
    override def toString:String = s"SparseMatrix(${rows.toSeq}, ${cols.toSeq}, ${data.toSeq}, $nRows, $nCols)"
  }

  object SparseMatrix {
    def forMoment(gramMatrix: GramMatrix[_, _], momentIndex: Int, factor: Double = 1.0): SparseMatrix = {
      import gramMatrix.{matrixSize => d}
      val rows = metal.mutable.Buffer.empty[Int]
      val cols = metal.mutable.Buffer.empty[Int]
      val data = metal.mutable.Buffer.empty[Double]
      cforRange(0 until d) { r =>
        cforRange(0 until d) { c =>
          // matlab has row-major indexing (not that it counts for symmetric matrices...)
          if (gramMatrix.momentIndex(r, c) == momentIndex) {
            rows += r
            cols += c
            data += gramMatrix.phase(r, c).toInt.toDouble * factor
          }
        }
      }
      SparseMatrix(rows.toArray, cols.toArray, data.toArray, d, d)
    }
  }

}