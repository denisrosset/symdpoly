package net.alasc.symdpoly
package solvers

import spire.syntax.cfor.cforRange

object UpperTriangular {

  case class SparseMatrix(rows: Array[Int], cols: Array[Int], data: Array[Double]) {
    def nEntries: Int = rows.length
    override def toString:String = s"SparseMatrix(${rows.toSeq}, ${cols.toSeq}, ${data.toSeq})"
  }

  object SparseMatrix {
    def forMoment(gramMatrix: GramMatrix[_, _], momentIndex: Int, factor: Double): SparseMatrix = {
      import gramMatrix.matrixSize
      val rows = metal.mutable.Buffer.empty[Int]
      val cols = metal.mutable.Buffer.empty[Int]
      val data = metal.mutable.Buffer.empty[Double]
      cforRange(0 until matrixSize) { c =>
        cforRange(0 to c) { r =>
          if (gramMatrix.momentIndex(r, c) == momentIndex) {
            rows += r
            cols += c
            data += gramMatrix.phase(r, c).toInt.toDouble * factor
          }
        }
      }
      SparseMatrix(rows.toArray, cols.toArray, data.toArray)
    }
  }

}
