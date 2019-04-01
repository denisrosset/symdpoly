package net.alasc.symdpoly
package sdp

import cats.kernel.Eq
import spire.algebra.{Group, Monoid, Semigroup}
import spire.random.Size
import spire.syntax.cfor._

import scalin.immutable.{Mat, MatEngine, Vec}
import spire.std.double._

import scalin.Sparse
import scalin.immutable.dense._

import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.symdpoly.algebra.Morphism
import net.alasc.symdpoly.math.{GenPerm, Phase, Phases}
import net.alasc.syntax.all._

case class BlockElement(basisIndex: Int, r: Int, c: Int, realPart: Double, complexPart: Double)

/** Describes a SDP constraint block expressed as a linear combination of basis elements on real scalar variables.
  *
  * The block is described as a sparse tensor by a sequence of (basisIndex, rowIndex, colIndex, coefficient) tuples.
  *
  * @param size         The block is a matrix of shape size x size
  * @param basisSize    Number of elements
  * @param basisIndices Index of basis element
  * @param rowIndices   Row index
  * @param colIndices   Column index
  * @param coefficients Real coefficient
  */
case class Block(size: Int,
                 basisSize: Int,
                 basisIndices: Array[Int],
                 rowIndices: Array[Int],
                 colIndices: Array[Int],
                 coefficients: Array[Double]) {
  def nEntries: Int = basisIndices.length
}

object Block {

  /** Direct sum of blocks in the same SDP relaxation (i.e. same number of basis elements). */
  implicit def directSum(basisSize: Int): Monoid[Block] = new Monoid[Block] {
    def empty: Block = Block(0, basisSize, Array.empty, Array.empty, Array.empty, Array.empty)
    def combine(x: Block, y: Block): Block = {
      require(x.basisSize == basisSize && y.basisSize == basisSize)
      Block(x.size + y.size, basisSize, x.basisIndices ++ y.basisIndices,
        x.rowIndices ++ y.rowIndices.map(_ + x.size), x.colIndices ++ y.colIndices.map(_ + y.size),
        x.coefficients ++ y.coefficients)
    }
  }

  /** Constructs a block using real coefficients only. */
  def realBlock(size: Int, basisSize: Int, elements: Seq[BlockElement]): Block =
    Block(size,
      basisSize,
      elements.map(_.basisIndex).toArray,
      elements.map(_.r).toArray,
      elements.map(_.c).toArray,
      elements.map(_.realPart).toArray
    )

  /** Constructs a block using a real encoding of its complex coefficients. */
  def complexBlock(size: Int, basisSize: Int, elements: Seq[BlockElement]): Block = {
    def realPart(i: Int, r: Int, c: Int, a: Double) = Seq(
      BlockElement(i, r*2, c*2, a, 0),
      BlockElement(i, r*2+1, c*2+1, a, 0)
    )
    def imagPart(i: Int, r: Int, c: Int, b: Double) = Seq(
      BlockElement(i, r*2, c*2+1, -b, 0),
      BlockElement(i, r*2+1, c*2, b, 0)
    )
    val complexEncoding = elements.flatMap {
      case BlockElement(i, r, c, 0.0, b) => imagPart(i, r, c, b)
      case BlockElement(i, r, c, a, 0.0) => realPart(i, r, c, a)
      case BlockElement(i, r, c, a, b) => realPart(i, r, c, a) ++ imagPart(i, r, c, b)
    }
    Block(size * 2,
      basisSize,
      elements.map(_.basisIndex).toArray,
      elements.map(_.r).toArray,
      elements.map(_.c).toArray,
      elements.map(_.realPart).toArray
    )
  }

  /** Constructs a SDP block from a series of indices.
    *
    * Assumes that no two elements have the same (dualIndex, r, c) value.
    */
  def apply(size: Int, basisSize: Int, elements: Seq[BlockElement]): Block =
    if (elements.forall(_.complexPart == 0))
      realBlock(size, basisSize, elements)
    else
      complexBlock(size, basisSize, elements)

}
