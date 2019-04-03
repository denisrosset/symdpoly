package net.alasc.symdpoly.math

import spire.syntax.cfor._
import scala.annotation.tailrec

import scalin.immutable.{CSCMat, Mat}

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

}

/** Sparse matrix of Double coefficients represented by triplets (rowIndex, colIndex, coefficients).
  *
  * In the normal form, indices are sorted first by column, then by row, and there are no duplicates
  * nor coefficients equal to zero.
  */
class DoubleCOOMat(val nRows: Int,
                   val nCols: Int,
                   private[symdpoly] val rowIndices: Array[Int],
                   private[symdpoly] val colIndices: Array[Int],
                   private[symdpoly] val data: Array[Double],
                   private[symdpoly] var nEntries: Int,
                   private[symdpoly] var isNormal: Boolean
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

  def vertcat(nCols: Int, matrices: DoubleCOOMat*): DoubleCOOMat = {
    require(matrices.forall(_.nCols == nCols))
    val n = matrices.map(_.nEntries).sum
    val nRows = matrices.map(_.nRows).sum
    val rowIndices = new Array[Int](n)
    val colIndices = new Array[Int](n)
    val data = new Array[Double](n)
    def rec(i: Int, posShift: Int, rowShift: Int): Unit =
      if (i < matrices.length) {
        val mat: DoubleCOOMat = matrices(i)
        cforRange(0 until mat.nEntries) { j =>
          rowIndices(posShift + j) = mat.rowIndices(j) + rowShift
        }
        Array.copy(mat.colIndices, 0, colIndices, posShift, mat.nEntries)
        Array.copy(mat.data, 0, data, posShift, mat.nEntries)
        rec(i + 1, posShift + mat.nEntries, rowShift + mat.nRows)
      }
    rec(0, 0, 0)
    DoubleCOOMat(nRows, nCols, rowIndices, colIndices, data)
  }

  def horzcat(nRows: Int, matrices: DoubleCOOMat*): DoubleCOOMat = {
    require(matrices.forall(_.nRows == nRows))
    val n = matrices.map(_.nEntries).sum
    val nCols = matrices.map(_.nCols).sum
    val rowIndices = new Array[Int](n)
    val colIndices = new Array[Int](n)
    val data = new Array[Double](n)
    def rec(i: Int, posShift: Int, colShift: Int): Unit =
      if (i < matrices.length) {
        val mat: DoubleCOOMat = matrices(i)
        cforRange(0 until mat.nEntries) { j =>
          colIndices(posShift + j) = mat.colIndices(j) + colShift
        }
        Array.copy(mat.rowIndices, 0, rowIndices, posShift, mat.nEntries)
        Array.copy(mat.data, 0, data, posShift, mat.nEntries)
        rec(i + 1, posShift + mat.nEntries, colShift + mat.nCols)
      }
    rec(0, 0, 0)
    DoubleCOOMat(nRows, nCols, rowIndices, colIndices, data)
  }

}
