package net.alasc.symdpoly
package solvers

import spire.syntax.cfor.cforRange

object LowerTriangular {

  case class SparseMatrix(rows: Array[Int], cols: Array[Int], data: Array[Double]) {
    def nTerms: Int = rows.length
    override def toString:String = s"SparseMatrix(${rows.toSeq}, ${cols.toSeq}, ${data.toSeq})"
  }

  object SparseMatrix {

    def forMoment(gramMatrix: GramMatrix[_, _], momentIndex: Int, factor: Double = 1.0): SparseMatrix = {
      import gramMatrix.matrixSize
      val rows = metal.mutable.Buffer.empty[Int]
      val cols = metal.mutable.Buffer.empty[Int]
      val data = metal.mutable.Buffer.empty[Double]
      cforRange(0 until matrixSize) { r =>
        cforRange(0 to r) { c =>
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
