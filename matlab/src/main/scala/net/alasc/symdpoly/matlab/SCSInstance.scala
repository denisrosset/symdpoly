package net.alasc.symdpoly.matlab

import spire.syntax.cfor._

import com.jmatio.io.MatFileWriter
import com.jmatio.types.{MLArray, MLDouble, MLSparse, MLStructure}
import net.alasc.symdpoly.solvers.Instance
import net.alasc.symdpoly.{GramMatrix, Relaxation, algebra}
import net.alasc.symdpoly.algebra.Phased.syntax._

/** Data provided in the format of https://github.com/bodono/scs-matlab */
class SCSInstance(val relaxation: Relaxation[_, _, _]) extends Instance {
  import SCSInstance.{SparseMatrix, SparseVector}
  import relaxation.{gramMatrix, objectiveVector}
  import gramMatrix.{matrixSize => d}
  // TODO require(gramMatrix.momentSet(0).isOne, "Error: empty/one monomial not part of the relaxation")
  val m: Int = gramMatrix.nUniqueMonomials - 1 // number of dual variables
  val n: Int = d * (d + 1) / 2

  val objShift = cycloToDouble(objectiveVector(0)) // constant in objective not supported

  val c = Array.tabulate(m)(i => -cycloToDouble(objectiveVector(i + 1)))

  val b = SparseVector.forMoment(gramMatrix, 0, 1.0)
  def aMatrix: SparseMatrix = {
      val columns = Array.tabulate(m)(c => SparseVector.forMoment(gramMatrix, c + 1, -1.0))
      val rows = columns.flatMap(_.indices)
      val cols = columns.zipWithIndex.flatMap { case (col, c) => Array.fill(col.nEntries)(c) }
      val data = columns.flatMap(_.data)
      SparseMatrix(rows, cols, data, n, m)
  }

  val a = aMatrix

  case class K(f: Int, l: Int, q: Array[Int], s: Array[Int], ep: Int, ed: Int, p: Array[Double])

  val k = K(0, 0, Array(), Array(d), 0, 0, Array())

  def writeFile(fileName: String): Unit = {
    val file = new java.io.File(fileName)
    val mlCones = new MLStructure("cones", Array(1, 1))
    mlCones.setField("f", new MLDouble(null, Array(k.f.toDouble), 1))
    mlCones.setField("l", new MLDouble(null, Array(k.l.toDouble), 1))
    mlCones.setField("q", new MLDouble(null, k.q.map(_.toDouble), 1))
    mlCones.setField("s", new MLDouble(null, k.s.map(_.toDouble), 1))
    mlCones.setField("ep", new MLDouble(null, Array(k.ep.toDouble), 1))
    mlCones.setField("ed", new MLDouble(null, Array(k.ed.toDouble), 1))
    mlCones.setField("p", new MLDouble(null, k.p, 1))

    val mlc = new MLDouble("c", c, m)
    val mlb = new MLDouble("b", Array.fill(b.length)(0.0), b.length)
    cforRange(0 until b.nEntries) { i =>
      mlb.set(b.data(i), b.indices(i), 0)
    }
    val mlA = new MLSparse("A", Array(n, m), 0, a.nEntries)
    cforRange(0 until a.nEntries) { i =>
      mlA.set(a.data(i), a.rows(i), a.cols(i))
    }
    val mlObjShift = new MLDouble("objShift", Array(objShift), 1)
    val list = new java.util.ArrayList[MLArray]()
    val mlData = new MLStructure("data", Array(1, 1))
    mlData.setField("A", mlA)
    mlData.setField("b", mlb)
    mlData.setField("c", mlc)
    list.add(mlData)
    list.add(mlObjShift)
    list.add(mlCones)
    new MatFileWriter(file, list)
  }

}

object SCSInstance {

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
        cforRange(c until d) { r =>
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

}