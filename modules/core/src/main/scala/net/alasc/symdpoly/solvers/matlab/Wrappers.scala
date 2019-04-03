package net.alasc.symdpoly
package solvers
package matlab

import net.alasc.symdpoly.math.DoubleCSCMat
import us.hebi.matlab.mat.format.Mat5WriteUtil._
import us.hebi.matlab.mat.format.{Mat5, Mat5Serializable, Mat5Type}
import us.hebi.matlab.mat.types.{AbstractArray, MatlabType, Sink}
import spire.syntax.cfor._
import scalin.immutable.{Mat, Vec}

abstract class DoubleMatWrapper(val matrix: Mat[Double]) extends AbstractArray(Mat5.dims(matrix.nRows, matrix.nCols)) with Mat5Serializable with Mat5Serializable.Mat5Attributes {

  protected def getMat5DataSize: Int

  def getMat5Size(name: String): Int = Mat5.MATRIX_TAG_SIZE + computeArrayHeaderSize(name, this) + getMat5DataSize

  protected def writeMat5Data(sink: Sink): Unit

  def writeMat5(name: String, isGlobal: Boolean, sink: Sink): Unit = {
    writeMatrixTag(name, this, sink)
    writeArrayHeader(name, isGlobal, this, sink)
    writeMat5Data(sink)
  }

  def isLogical = false
  def isComplex: Boolean = false
  def subHashCode(): Int = matrix.hashCode

  protected def subEqualsGuaranteedSameClass(otherGuaranteedSameClass: Any): Boolean = {
    val other = otherGuaranteedSameClass.asInstanceOf[DoubleMatWrapper]
    this.matrix == other.matrix
  }

  def close(): Unit = ()

}

class DoubleCSCMatWrapper(override val matrix: DoubleCSCMat) extends DoubleMatWrapper(matrix) {
  import matrix.{colPtrs, data, nCols, rowIndices}

  protected def getMat5DataSize: Int =
    Mat5Type.Int32.computeSerializedSize(getNumRowIndices) +
      Mat5Type.Int32.computeSerializedSize(getNumColIndices) +
      Mat5Type.Double.computeSerializedSize(data.length)

  protected def writeMat5Data(sink: Sink): Unit = { // Row indices (MATLAB requires at least 1 entry)
    Mat5Type.Int32.writeTag(getNumRowIndices, sink)
    if (data.length == 0) sink.writeInt(0) else sink.writeInts(rowIndices, 0, getNumRowIndices)
    Mat5Type.Int32.writePadding(getNumRowIndices, sink)
    // Column indices
    Mat5Type.Int32.writeTag(getNumColIndices, sink)
    sink.writeInts(colPtrs, 0, getNumColIndices)
    Mat5Type.Int32.writePadding(getNumColIndices, sink)
    // Non-zero values
    Mat5Type.Double.writeTag(data.length, sink)
    sink.writeDoubles(data, 0, data.length)
    Mat5Type.Double.writePadding(data.length, sink)
  }

  def getType = MatlabType.Sparse
  def getNzMax: Int = spire.math.max(1, data.length)

  private[this] def getNumRowIndices = getNzMax
  private[this] def getNumColIndices = nCols + 1

}

class DoubleDenseMatWrapper(matrix: Mat[Double]) extends DoubleMatWrapper(matrix) {

  protected def getMat5DataSize: Int = Mat5Type.Double.computeSerializedSize(matrix.nRows * matrix.nCols)

  import us.hebi.matlab.mat.format.Mat5Type
  import java.io.IOException

  @throws[IOException]
  protected def writeMat5Data(sink: Sink): Unit = {
    // Real data in column major format
    Mat5Type.Double.writeTag(matrix.nRows * matrix.nCols, sink)
    cforRange(0 until matrix.nCols) { col =>
      cforRange(0 until matrix.nRows) { row =>
        sink.writeDouble(matrix(row, col))
      }
    }
    Mat5Type.Double.writePadding(matrix.nRows * matrix.nCols, sink)
  }

  def getType: MatlabType = MatlabType.Double

  def getNzMax: Int = 0

}