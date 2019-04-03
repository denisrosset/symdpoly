package net.alasc.symdpoly.math

import spire.syntax.cfor._
import scala.annotation.tailrec

import us.hebi.matlab.mat.types.{AbstractArray, MatlabType, Sink, Array => MFLArray}
import us.hebi.matlab.mat.format.{Mat5, Mat5Serializable, Mat5Type}
import us.hebi.matlab.mat.format.Mat5WriteUtil._

class DoubleCSCMatWrapper(val matrix: DoubleCSCMat) extends AbstractArray(Mat5.dims(matrix.nRows, matrix.nCols)) with Mat5Serializable with Mat5Serializable.Mat5Attributes {
  import matrix.{nRows, nCols, colPtrs, rowIndices, data}

  def getMat5Size(name: String): Int = Mat5.MATRIX_TAG_SIZE + computeArrayHeaderSize(name, this) + getMat5DataSize

  protected def getMat5DataSize: Int =
    Mat5Type.Int32.computeSerializedSize(getNumRowIndices) +
      Mat5Type.Int32.computeSerializedSize(getNumColIndices) +
      Mat5Type.Double.computeSerializedSize(data.length)

  import java.io.IOException

  def writeMat5(name: String, isGlobal: Boolean, sink: Sink): Unit = {
    writeMatrixTag(name, this, sink)
    writeArrayHeader(name, isGlobal, this, sink)
    writeMat5Data(sink)
  }

  protected def writeMat5Data(sink: Sink): Unit = { // Row indices (MATLAB requires at least 1 entry)
    Mat5Type.Int32.writeTag(getNumRowIndices, sink)
    if (data.length == 0) sink.writeInt(0)
    else sink.writeInts(rowIndices, 0, getNumRowIndices)
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
  def isLogical = false
  def isComplex: Boolean = false
  def getNzMax: Int = spire.math.max(1, data.length)
  private[this] def getNumRowIndices = getNzMax
  private[this] def getNumColIndices = nCols + 1

  def close(): Unit = ()

  def subHashCode(): Int = hashCode

  protected def subEqualsGuaranteedSameClass(otherGuaranteedSameClass: Any): Boolean = {
    val other = otherGuaranteedSameClass.asInstanceOf[DoubleCSCMatWrapper]
    this.matrix == other.matrix
  }

}

/** A compressed sparse column matrix, as used in Matlab, etc..,
  * see [[https://en.wikipedia.org/wiki/Sparse_matrix#Compressed_sparse_column_.28CSC_or_CCS.29]]
  */
class DoubleCSCMat(val nRows: Int,
                   val nCols: Int,
                   private[symdpoly] val colPtrs: Array[Int],
                   private[symdpoly] val rowIndices: Array[Int],
                   private[symdpoly] val data: Array[Double]) extends scalin.immutable.Mat[Double] {

  protected def locate(row: Int, col: Int): Int = {
    import spire.std.int._
    val start = colPtrs(col)
    val end = colPtrs(col + 1)
    if (start == end)
      ~start
    else
      spire.math.Searching.search(rowIndices, row, start, end - 1)
  }

  def apply(row: Int, col: Int): Double = {
    if (row >= nRows || col >= nCols || row < 0 || col < 0)
      throw new IndexOutOfBoundsException()
    val ind = locate(row, col)
    if (ind < 0) 0.0
    else data(ind)
  }

  def toMatlab: DoubleCSCMatWrapper = new DoubleCSCMatWrapper(this)

}

/** Sparse matrix of Double coefficients represented by triplets (rowIndex, colIndex, coefficients).
  *
  * In the normal form, indices are sorted first by column, then by row, and there are no duplicates
  * nor coefficients equal to zero.
  */
class DoubleCOOMat(val nRows: Int,
                   val nCols: Int,
                   private[this] val rowIndices: Array[Int],
                   private[this] val colIndices: Array[Int],
                   private[this] val data: Array[Double],
                   private[this] var nEntries: Int,
                   private[this] var isNormal: Boolean
                   ) extends scalin.immutable.Mat[Double] {


  @inline private[this] def swap[@specialized T](a: Array[T], i: Int, j: Int): Unit = {
    val t = a(i)
    a(i) = a(j)
    a(j) = t
  }

  def normalize(): Unit = if (!isNormal) {
    /** Sorts the elements by column first, then by row. Does not compact duplicates. */
    def shellSort(): Unit = {
      @inline def compareIndices(x: Int, y: Int): Boolean = java.lang.Integer.compare(colIndices(x), colIndices(y)) match {
        case -1 => true
        case 0 => rowIndices(x) < rowIndices(y)
        case 1 => false
      }
      var h = 1
      while (h < nEntries / 3) h = 3 * h + 1
      while (h >= 1) {
        cforRange(h until nEntries) { i =>
          var j = i
          while (j >= h && compareIndices(j, j - h)) {
            swap(data, j, j - h)
            swap(rowIndices, j, j - h)
            swap(colIndices, j, j - h)
            j -= h
          }
        }
        h /= 3
      }
    }

    /** Compacts successive elements with same (row, col). */
    def compact(): Unit = {
      @tailrec def rec(i: Int, // current empty storage position
                       r: Int, // current row
                       c: Int, // current col
                       a: Double, // current sum
                       j: Int // element to inspect
                      ): Int =
        if (j == nEntries || r != rowIndices(j) || c != colIndices(j)) {
          if (a == 0.0) {
            if (j == nEntries)
              i
            else
              rec(i, rowIndices(j), colIndices(j), data(j), j + 1) // reuse storage position i
          } else {
            rowIndices(i) = r
            colIndices(i) = c
            data(i) = a
            if (j == nEntries)
              i + 1 // number of stored elements
            else
              rec(i + 1, rowIndices(j), colIndices(j), data(j), j + 1)
          }
        } else rec(i, r, c, a + data(j), j + 1)

      if (nEntries > 0) {
        nEntries = rec(0, rowIndices(0), colIndices(0), data(0), 1)
      }
    }
    shellSort()
    compact()
    isNormal = true
  }

  def apply(r: Int, c: Int): Double = {
    if (r >= nRows || c >= nCols || r < 0 || c < 0)
      throw new IndexOutOfBoundsException()
    if (isNormal) {
      @inline def compareIndices(i: Int): Int = java.lang.Integer.compare(colIndices(i), c) match {
        case 0 => java.lang.Integer.compare(rowIndices(i), r)
        case comp => comp
      }
      // binary search lifted from spire.math.Searching
      var first = 0
      var last = nEntries - 1
      while (first <= last) {
        val middle = (first + last) >>> 1
        val comp = java.lang.Integer.compare(colIndices(middle), c) match {
          case 0 => java.lang.Integer.compare(rowIndices(middle), r)
          case res => res
        }
        if (comp < 0) first = middle + 1
        else if (comp > 0) last = middle - 1
        else return data(middle)
      }
      0.0
    } else {
      @tailrec def rec(s: Double, i: Int): Double =
        if (i == nEntries) s
        else if (rowIndices(i) == r && colIndices(i) == c) rec(s + data(i), i + 1)
        else rec(s, i + 1)
      rec(0.0, 0)
    }
  }

  def toDoubleCSCMat: DoubleCSCMat = {
    normalize()
    val data1 = java.util.Arrays.copyOfRange(data, 0, nEntries)
    val rowIndices1 = java.util.Arrays.copyOfRange(rowIndices, 0, nEntries)
    val colPtrs = new Array[Int](nCols + 1)
    @tailrec def fillColPtrs(c: Int, // Current column, has been already filled
                             i: Int  // Index of new position to test
                            ): Unit =
      if (c <= nCols) {
        colPtrs(c) = i
        if (i == nEntries) { // no more elements, colPtrs should be equal to n
          if (c < nCols)
            fillColPtrs(c + 1, i)
        }
        else if (c < colIndices(i)) // more elements, but jumped columns
          fillColPtrs(c + 1, i) // catch up the next column
        else if (c == colIndices(i)) // we are on that column
          fillColPtrs(c + 1, i + 1) // go to the next column, this one is fixed
        else // we are already at the next column, but this is a run of elements for the previous column
          fillColPtrs(c, i + 1)
      } else sys.error("Inconsistent data")
    fillColPtrs(0, 0)
    new DoubleCSCMat(nRows, nCols, colPtrs, rowIndices1, data1)
  }

}

object DoubleCOOMat {

  def applyNormalized(nRows: Int, nCols: Int, rowIndices: Array[Int], colIndices: Array[Int], data: Array[Double]): DoubleCOOMat =
    new DoubleCOOMat(nRows, nCols, rowIndices, colIndices, data, data.length, true)

  def apply(nRows: Int, nCols: Int, rowIndices: Array[Int], colIndices: Array[Int], data: Array[Double]): DoubleCOOMat =
    new DoubleCOOMat(nRows, nCols, rowIndices, colIndices, data, data.length, false)

}