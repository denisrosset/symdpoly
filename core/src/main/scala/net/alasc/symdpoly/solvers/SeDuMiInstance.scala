package net.alasc.symdpoly
package solvers

import spire.syntax.cfor.cforRange

import com.jmatio.io.MatFileWriter
import com.jmatio.types.{MLArray, MLDouble, MLSparse, MLStructure}

class SeDuMiInstance(relaxation: Relaxation[_, _, _]) extends Instance {
  import SeDuMiInstance.{SparseMatrix, SparseVector}
  import relaxation.{gramMatrix, objectiveVector}
  import gramMatrix.matrixSize
  require(gramMatrix.momentSet(0).isOne, "Error: empty/one monomial not part of the relaxation")

  val m: Int = gramMatrix.nUniqueMonomials - 1 // number of dual variables
  val n: Int = matrixSize * matrixSize

  case class K(f: Int, l: Int, q: Array[Int], r: Array[Int], s: Array[Int])

  val k = K(0, 0, Array(), Array(), Array(matrixSize))

  val c = SparseVector.forMoment(gramMatrix, 0)

  def aMatrix: SparseMatrix = {
    val columns = Array.tabulate(m)(c => SparseVector.forMoment(gramMatrix, c + 1, -1.0))
    val rows = columns.flatMap(_.indices)
    val cols = columns.zipWithIndex.flatMap { case (col, c) => Array.fill(col.nEntries)(c) }
    val data = columns.flatMap(_.data)
    SparseMatrix(rows, cols, data, n, m)
  }

  val a = aMatrix

  val b = Array.tabulate(m)(i => cycloToDouble(objectiveVector(i + 1)))

  val objShift = cycloToDouble(objectiveVector(0)) // constant in objective not supported

  def writeFile(fileName: String): Unit = {
    val file = new java.io.File(fileName)
    val dataK = new MLStructure("K", Array(1, 1))
    dataK.setField("f", new MLDouble(null, Array(k.f.toDouble), 1))
    dataK.setField("l", new MLDouble(null, Array(k.l.toDouble), 1))
    dataK.setField("q", new MLDouble(null, k.q.map(_.toDouble), 1))
    dataK.setField("r", new MLDouble(null, k.r.map(_.toDouble), 1))
    dataK.setField("s", new MLDouble(null, k.s.map(_.toDouble), 1))
    val dataB = new MLDouble("b", b, m)
    val dataC = new MLSparse("c", Array(c.length, 1), 0, c.nEntries)
    cforRange(0 until c.nEntries) { i =>
      dataC.set(c.data(i), c.indices(i), 0)
    }
    val dataA = new MLSparse("A", Array(n, m), 0, a.nEntries)
    cforRange(0 until a.nEntries) { i =>
      dataA.set(a.data(i), a.rows(i), a.cols(i))
    }
    val dataObjShift = new MLDouble("objShift", Array(objShift), 1)
    val list = new java.util.ArrayList[MLArray]()
    list.add(dataK)
    list.add(dataB)
    list.add(dataC)
    list.add(dataA)
    list.add(dataObjShift)
    new MatFileWriter(file, list)
  }

}

object SeDuMiInstance {

  case class SparseVector(indices: Array[Int], data: Array[Double], length: Int) {
    def nEntries: Int = indices.length
  }

  object SparseVector {
    def forMoment(gramMatrix: GramMatrix[_, _], momentIndex: Int, factor: Double = 1.0): SparseVector = {
      import gramMatrix.matrixSize
      val indices = metal.mutable.Buffer.empty[Int]
      val data = metal.mutable.Buffer.empty[Double]
      cforRange(0 until matrixSize) { c =>
        cforRange(0 until matrixSize) { r =>
          // matlab has row-major indexing (not that it counts for symmetric matrices...)
          val index = r + c * matrixSize
          if (gramMatrix.momentIndex(r, c) == momentIndex) {
            indices += index
            data += gramMatrix.phase(r, c).toInt.toDouble * factor
          }
        }
      }
      SparseVector(indices.toArray, data.toArray, matrixSize * matrixSize)
    }
  }

  case class SparseMatrix(rows: Array[Int], cols: Array[Int], data: Array[Double], nRows: Int, nCols: Int) {
    def nEntries: Int = rows.length
    override def toString:String = s"SparseMatrix(${rows.toSeq}, ${cols.toSeq}, ${data.toSeq}, $nRows, $nCols)"
  }

}
