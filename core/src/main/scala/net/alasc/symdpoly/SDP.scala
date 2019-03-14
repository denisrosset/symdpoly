package net.alasc.symdpoly

import java.io.{BufferedWriter, FileWriter, StringWriter, Writer}

import spire.syntax.cfor.cforRange

import scalin.immutable.{Mat, Vec}
import spire.std.double._
import scalin.immutable.dense._

/** Description of an semidefinite program extended dual.
  *
  * The conic linear program is given by:
  *
  *   maximize   sum_i objToMaximize(i) * y(i)
  *   over real y(0), ..., y(m-1)
  *
  *   subject to
  *
  *   y(0) == 1
  *   for all j: sum_i y(i) blocks(j).basis(i) >= 0
  *   eqA * y == 0
  *   ineqA * y >= 0 (component-wise)
  */
case class SDP(direction: Direction, obj: Vec[Double], blocks: Seq[SDP.Block], eqA: Mat[Double], ineqA: Mat[Double]) {

  def convertEqualitiesToInequalities: SDP = SDP(direction, obj, blocks, Mat.zeros[Double](0, obj.length), ineqA vertcat eqA vertcat (-eqA))

  def convertInequalitiesToBlock: SDP = {
    val entries = for {
      r <- 0 until ineqA.nRows
      c <- 0 until ineqA.nCols
      coeff = ineqA(r, c) if coeff != 0
    } yield (c, r, r, coeff)
    val newBlock = SDP.Block(ineqA.nRows, entries.map(_._1).toArray, entries.map(_._2).toArray, entries.map(_._3).toArray, entries.map(_._4).toArray)
    SDP(direction, obj, blocks :+ newBlock, eqA, Mat.zeros[Double](0, obj.length))
  }

  def mergeBlocks: SDP = {
    val shifts = blocks.map(_.size).scanLeft(0)(_ + _)
    val basisIndex = blocks.toArray.flatMap(_.basisIndex)
    val rowIndex = (blocks zip shifts).toArray.flatMap { case (block, shift) => block.rowIndex.map(_ + shift) }
    val colIndex = (blocks zip shifts).toArray.flatMap { case (block, shift) => block.colIndex.map(_ + shift) }
    val coeffs = blocks.toArray.flatMap(_.coeffs)
    val size = blocks.map(_.size).reduce(_ + _)
    val newBlock = SDP.Block(size, basisIndex, rowIndex, colIndex, coeffs)
    SDP(direction, obj, blocks :+ newBlock, eqA, ineqA)
  }

  def sdpa: solvers.SDPAInstance = solvers.SDPAInstance(this)

  def mosek: solvers.MosekInstance = solvers.MosekInstance(this)

}

object SDP {

  case class Block(size: Int, basisIndex: Array[Int], rowIndex: Array[Int], colIndex: Array[Int], coeffs: Array[Double]) {
    def nEntries: Int = basisIndex.length
  }

  object Block {
    def realBlock(size: Int, elements: Seq[BlockElement]): SDP.Block =
      Block(size, elements.map(_.dualIndex).toArray, elements.map(_.r).toArray, elements.map(_.c).toArray, elements.map(_.realPart).toArray)

    def complexBlock(size: Int, elements: Seq[BlockElement]): SDP.Block = {
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
      realBlock(size * 2, complexEncoding)
    }

    /** Constructs a SDP block from a series of indices.
      *
      * Assumes that no two elements have the same (dualIndex, r, c) value.
      */
    def apply(size: Int, elements: Seq[BlockElement]): Block =
      if (elements.forall(_.complexPart == 0)) realBlock(size, elements) else complexBlock(size, elements)

  }

}
