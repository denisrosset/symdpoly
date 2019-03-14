package net.alasc.symdpoly
package matlab

import java.util.{ArrayList => JavaArrayList, Collection => JavaCollection}

import spire.syntax.cfor._

import scalin.immutable.{Mat, Vec}

import com.jmatio.io.MatFileWriter
import com.jmatio.types.{MLArray, MLCell, MLChar, MLDouble, MLSparse, MLStructure}
import syntax.phased._
import spire.std.double._

trait MatlabFormat {

  def data: Struct

  def writeFile(filename: String): Unit = data.write(filename)

}

sealed trait MatlabData {

  def data(name: Option[String]): MLArray

}

case class Struct(elements: (String, MatlabData)*) extends MatlabData {

  def data(name: Option[String]): MLArray = {
    val mlA = new MLStructure(name.getOrElse(null), Array(1, 1))
    for ( (fieldName, fieldValue) <- elements )
      mlA.setField(fieldName, fieldValue.data(Some(fieldName)))
    mlA
  }

  def exported: JavaCollection[MLArray] = {
    val list = new JavaArrayList[MLArray]
    for ( (fieldName, fieldValue) <- elements )
      list.add(fieldValue.data(Some(fieldName)))
    list
  }

  def write(filename: String): Unit =
    new MatFileWriter(filename, exported)

}

case class Char(s: String) extends MatlabData {
  def data(name: Option[String]): MLArray = {
    new MLChar(name.getOrElse(null), s)
  }
}

case class CellArray(mat: Mat[MatlabData]) extends MatlabData {
  def data(name: Option[String]): MLArray = {
    val mlA = new MLCell(name.getOrElse(null), Array(mat.nRows, mat.nCols))
    cforRange(0 until mat.nRows) { r =>
      cforRange(0 until mat.nCols) { c =>
        mlA.set(mat(r, c).data(None), r, c)
      }
    }
    mlA
  }
}

case class Matrix(mat: Mat[Double]) extends MatlabData {
  def data(name: Option[String]): MLArray = {
    val matlabName = name.getOrElse(null)
    val dims = Array(mat.nRows, mat.nCols)
    mat match {
      case m: scalin.immutable.CSCMat[Double] =>
        val mlA = new MLSparse(matlabName, dims, 0, m.rowIndices.length)
        cforRange(0 until m.colPtrs.length - 1) { c =>
          val start = m.colPtrs(c)
          val stopBefore = m.colPtrs(c + 1)
          cforRange(start until stopBefore) { i =>
            val r = m.rowIndices(i)
            val e = m.data(i).asInstanceOf[Double]
            mlA.set(e, r, c)
          }
        }
        mlA
      case _ =>
        val packed = new Array[Double](mat.nRows * mat.nCols)
        var i = 0
        cforRange(0 until mat.nCols) { c =>
          cforRange(0 until mat.nRows) { r =>
            packed(i) = mat(r, c)
            i += 1
          }
        }
        new MLDouble(matlabName, packed, mat.nRows)
    }
  }
}

case class Vect(vec: Array[Double], isColVector: Boolean) extends MatlabData {
  def data(name: Option[String]): MLArray = {
    val nRows = if (isColVector) vec.length else 1
    new MLDouble(name.getOrElse(null), vec.toArray, nRows)
  }
}

object Vect {
  import scalin.immutable.dense._
  def row(array: Array[Double]): Vect = Vect(array, false)
  def col(array: Array[Double]): Vect = Vect(array, true)
  def row(vec: Vec[Double]): Vect = Vect(vec.toArray, false)
  def col(vec: Vec[Double]): Vect = Vect(vec.toArray, true)
  val emptyRow: Vect = row(Vec.zeros[Double](0))
  val emptyCol: Vect = col(Vec.zeros[Double](0))
}

case class Scalar(real: Double) extends MatlabData {
  def data(name: Option[String]): MLArray = new MLDouble(name.getOrElse(null), Array(real), 1)
}
