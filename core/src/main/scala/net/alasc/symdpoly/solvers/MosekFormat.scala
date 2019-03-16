package net.alasc.symdpoly
package solvers

import java.io.{BufferedWriter, FileWriter, PrintWriter, StringWriter, Writer}

import scalin.Sparse
import scalin.immutable.{Mat, Vec}
import spire.syntax.cfor._

import scalin.immutable.dense._
import scalin.syntax.all._

import MosekFormat._
import net.alasc.symdpoly.sdp.{Block, Program}

case class ObjFCoordElement(j: PSDVariable, r: Int, c: Int, real: Double)
case class ObjFCoord(elements: Seq[ObjFCoordElement]) {
  def writeData(writer: Writer): Unit = {
    writer.append("OBJFCOORD\n")
    writer.append(s"${elements.size}\n")
    for ( ObjFCoordElement(j, r, c, real) <- elements)
      writer.append(s"$j $r $c $real\n")
    writer.append("\n")
  }
}

case class PSDVarElement(n: Int)
case class PSDVar(elements: Seq[PSDVarElement]) {
  val N: Int = elements.length
  def writeData(writer: Writer): Unit = {
    writer.append("PSDVAR\n")
    writer.append(s"$N\n")
    for ( PSDVarElement(n) <- elements )
      writer.append(s"$n\n")
    writer.append("\n")
  }
}

case class VarElement(cone: String, n: Int)
case class Var(elements: Seq[VarElement]) {
  /** Number of variables. */
  val n = elements.map(_.n).sum
  /** Number of cones. */
  val k = elements.size
  def writeData(writer: Writer): Unit = {
    writer.append("VAR\n")
    writer.append(s"$n $k\n")
    for ( VarElement(cone, nCone) <- elements )
      writer.append(s"$cone $nCone\n")
    writer.append("\n")
  }
}

case class ConElement(cone: String, n: Int)
case class Con(elements: Seq[ConElement]) {
  val nCones: Int = elements.size
  val totalDimension: Int = elements.map(_.n).sum
  def writeData(writer: Writer): Unit = {
    writer.append("CON\n")
    writer.append(s"$totalDimension $nCones\n")
    for ( ConElement(cone, n) <- elements )
      writer.append(s"$cone $n\n")
    writer.append("\n")
  }
}

case class ObjACoordElement(j: ScalarVariable, real: Double)
case class ObjACoord(elements: Seq[ObjACoordElement]) {
  def writeData(writer: Writer): Unit = {
    writer.append("OBJACOORD\n")
    writer.append(s"${elements.size}\n")
    for ( ObjACoordElement(j, real) <- elements )
      writer.append(s"$j $real\n")
    writer.append("\n")
  }
}
case class FCoordElement(i: ScalarConstraint, j: PSDVariable, r: Int, c: Int, real: Double)
case class FCoord(elements: Seq[FCoordElement]) {
  def writeData(writer: Writer): Unit = {
    writer.append("FCOORD\n")
    writer.append(s"${elements.size}\n")
    for (FCoordElement(i, j, r, c, real) <- elements)
      writer.append(s"$i $j $r $c $real\n")
    writer.append("\n")
  }
}
case class ACoordElement(i: ScalarConstraint, j: ScalarVariable, real: Double)
case class ACoord(elements: Seq[ACoordElement]) {
  def writeData(writer: Writer): Unit = {
    writer.append("ACOORD\n")
    writer.append(s"${elements.size}\n")
    for (ACoordElement(i, j, real) <- elements)
      writer.append(s"$i $j $real\n")
    writer.append("\n")
  }
}
case class BCoordElement(i: ScalarConstraint, real: Double)
case class BCoord(elements: Seq[BCoordElement]) {
  def writeData(writer: Writer): Unit = {
    writer.append("BCOORD\n")
    writer.append(s"${elements.size}\n")
    for (BCoordElement(i, real) <- elements)
      writer.append(s"$i $real\n")
    writer.append("\n")
  }
}
case class ObjBCoord(real: Double) {
  def writeData(writer: Writer): Unit = {
    writer.append("OBJBCOORD\n")
    writer.append(s"$real\n")
    writer.append("\n")
  }
}
object MosekFormat {
  type ScalarConstraint = Int
  type PSDVariable = Int
  type ScalarVariable = Int
  def apply(sdp: Program): MosekFormat = {
    val psdVar = sdp.sdpCon.blocks.map(block => PSDVarElement(block.size))
    val `var` = Seq(VarElement("F", sdp.eqA.nRows), VarElement("L+", sdp.ineqA.nRows)).filter(_.n > 0)
    val shifts = Seq(0, sdp.eqA.nRows)
    val objFcoord = for {
      (block, j) <- sdp.sdpCon.blocks.zipWithIndex
      k <- 0 until block.nEntries
      i = block.basisIndices(k) if i == 0
      r = block.rowIndices(k)
      c = block.colIndices(k) if r >= c
      e = block.coefficients(k)
    } yield ObjFCoordElement(j, r, c, e)
    val objACoord = for {
      (mat, shift) <- Seq(sdp.eqA, sdp.ineqA) zip shifts
      r <- 0 until mat.nRows
      e = mat(r, 0) if e != 0
    } yield ObjACoordElement(r + shift, e)
    val fCoord = for {
      (block, j) <- sdp.sdpCon.blocks.zipWithIndex
      k <- 0 until block.nEntries
      i = block.basisIndices(k) if i > 0
      r = block.rowIndices(k)
      c = block.colIndices(k) if r >= c
      e = block.coefficients(k)
    } yield FCoordElement(i - 1, j, r, c, -e)
    val aCoord = for {
      (mat, shift) <- Seq(sdp.eqA, sdp.ineqA) zip shifts
      r <- 0 until mat.nRows
      c <- 1 until mat.nCols
      e = mat(r, c) if e != 0
    } yield ACoordElement(r + shift, c - 1, -e)
    val bCoord = for {
      i <- 1 until sdp.obj.length
      e = sdp.obj(i) if e != 0
    } yield BCoordElement(i - 1, -e)
    val con = Seq(ConElement("L=", sdp.obj.length - 1))
    val bObj = sdp.obj(0)
    MosekFormat(sdp.direction.reverse,
      PSDVar(psdVar),
      Var(`var`),
      Con(con),
      ObjFCoord(objFcoord),
      ObjACoord(objACoord),
      FCoord(fCoord),
      ACoord(aCoord),
      BCoord(bCoord),
      ObjBCoord(bObj)
    )
  }
}

/** Support for export to Mosek CBF file format
  *
  * Note that Mosek does not support PSDCON blocks, so we have to write the dual of
  * the natural form of the problem.
  *
  * Our original problem is of the form
  *
  * min/max \sum_i y_i b_i
  *
  * s.t. y_0 = 1
  *
  * \sum_i y_i A_i >= 0
  *
  * Aeq y = 0
  * Aineq y >= 0
  *
  * so the dual looks like
  *
  * max/min \sum_j Aj_0 Xj + ceq(:,0) xeq + cineq(:,0) xineq
  * s.t. Xj >=sdp 0 and xineq >=lp 0
  * \sum_j Aj_i X_j + Aeq(:,1:)' xeq + Aineq(:,1:)' ineq = -b_i
  */
case class MosekFormat(direction: Direction,
                       psdVar: PSDVar,
                       `var`: Var,
                       con: Con,
                       objFCoord: ObjFCoord,
                       objACoord: ObjACoord,
                       fCoord: FCoord,
                       aCoord: ACoord,
                       bCoord: BCoord,
                       objBCoord: ObjBCoord
                    ) extends TextFormat {

  def writeData(writer: Writer): Unit = {
    writer.append("VER\n")
    writer.append("1\n")
    writer.append("\n")
    writer.append("OBJSENSE\n")
    direction match {
      case Direction.Maximize => writer.append("MAX\n")
      case Direction.Minimize => writer.append("MIN\n")
    }
    writer.append("\n")
    psdVar.writeData(writer)
    `var`.writeData(writer)
    con.writeData(writer)
    objFCoord.writeData(writer)
    objACoord.writeData(writer)
    fCoord.writeData(writer)
    aCoord.writeData(writer)
    bCoord.writeData(writer)
    objBCoord.writeData(writer)
  }

}

case class MosekUnsupportedFormat(val sdp: Program) extends TextFormat {
  import sdp._

  val n: Int = obj.length - 1

  def writeVector(writer: Writer, vec: Vec[Double]): Unit = {
    val pairs = vec.toIndexedSeq.zipWithIndex.filterNot(_._1 == 0)
    val nel = pairs.size
    writer.append(s"$nel\n")
    pairs.foreach { case (c, i) => writer.append(s"$i $c\n") }
  }

  /** Line in the ACOORD block.
    *
    * @param i Scalar constraint index
    * @param j Scalar variable index
    * @param real Coefficient
    */
  case class ACoordElement(i: Int, j: Int, real: Double)

  /** Line in the BCOORD block.
    *
    * @param i Scalar constraint index
    * @param real Coefficient
    */
  case class BCoordElement(i: Int, real: Double)

  /** Line in the HCOORD block.
    *
    * @param i PSD constraint index
    * @param j Scalar variable index
    * @param r Row
    * @param c Column
    * @param real Coefficient
    */
  case class HCoordElement(i: Int, j: Int, r: Int, c: Int, real: Double)

  /** Line in the DCOORD block
    *
    * @param i PSD constraint index
    * @param r Row
    * @param c Column
    * @param real Coefficient
    */
  case class DCoordElement(i: Int, r: Int, c: Int, real: Double)

  case class PSDConstraints(sizes: Seq[Int], hCoord: Seq[HCoordElement], dCoord: Seq[DCoordElement])

  object PSDConstraints {

    def apply(blocks: Seq[Block]): PSDConstraints = {
      val hCoord = for {
        (block, i) <- blocks.zipWithIndex
        e <- 0 until block.nEntries
        j = block.basisIndices(e) if j > 0
        r = block.rowIndices(e)
        c = block.colIndices(e) if c <= r
        real = block.coefficients(e)
      } yield HCoordElement(i, j, r, c, real)
      val dCoord = for {
        (block, i) <- blocks.zipWithIndex
        e <- 0 until block.nEntries
        j = block.basisIndices(e) if j == 0
        r = block.rowIndices(e)
        c = block.colIndices(e) if c <= r
        real= block.coefficients(e)
      } yield DCoordElement(i, r, c, real)
      PSDConstraints(blocks.map(_.size), hCoord, dCoord)
    }

  }

  case class ScalarConstraint(cone: String, aCoord: Seq[ACoordElement], bCoord: Seq[BCoordElement]) {
    def m: Int = aCoord.size
    def isEmpty: Boolean = aCoord.isEmpty
  }

  object ScalarConstraint {

    def apply(cone: String, mat: Mat[Double]): ScalarConstraint = {
      val n = mat.nRows
      val bCoord = (0 until n).map(mat(_, 0)).zipWithIndex.filter(_._1 != 0).map { case (real, i) => BCoordElement(i, real) }
      val aCoord = for {
        r <- 0 until n
        c <- 1 until mat.nCols
        e = mat(r, c) if e != 0
      } yield ACoordElement(r, c - 1, e)
      ScalarConstraint(cone, aCoord, bCoord)
    }

  }

  def writeData(writer: Writer): Unit = {
    writer.append("VER\n")
    writer.append("1\n")
    writer.append("\n")
    writer.append("OBJSENSE\n")
    direction match {
      case Direction.Maximize => writer.append("MAX\n")
      case Direction.Minimize => writer.append("MIN\n")
    }
    writer.append("\n")
    writer.append("VAR\n")
    writer.append(s"$n 1\n")
    writer.append(s"F $n\n")
    writer.append("\n")
    // objective is b^obj + \sum_j a_j^obj x_j
    writer.append("OBJACOORD\n")
    writeVector(writer, obj(1 until obj.length))
    writer.append("\n")
    writer.append("OBJBCOORD\n")
    writer.append(s"${obj(0)}\n")
    writer.append("\n")
    val scalarConstraints = Seq(
      ScalarConstraint("L=", eqA),
      ScalarConstraint("L+", ineqA)
    ).filterNot(_.isEmpty)
    if (scalarConstraints.nonEmpty) {
      writer.append("CON\n")
      val k = scalarConstraints.size
      val m = scalarConstraints.map(_.m).sum
      writer.append(s"$m $k\n")
      scalarConstraints.foreach { sc =>
        writer.append(s"${sc.cone} ${sc.m}\n")
      }
      writer.append("\n")
      // g_i = sum_j a_ij x_j + b_i
      writer.append("ACOORD\n")
      scalarConstraints.foldLeft(0) {
        case (shift, sc) =>
          for (ACoordElement(i, j, real) <- sc.aCoord) {
            writer.append(s"${shift + i} $j $real\n")
          }
          shift + sc.m
      }
      writer.append("\n")
      writer.append("BCOORD\n")
      scalarConstraints.foldLeft(0) {
        case (shift, sc) =>
          for (BCoordElement(i, real) <- sc.bCoord) {
            writer.append(s"${shift + i} $real\n")
          }
          shift + sc.m
      }
      writer.append("\n")
    }
    if (sdpCon.blocks.nonEmpty) {
      val psd = PSDConstraints(sdpCon.blocks)
      writer.append("PSDCON\n")
      writer.append(s"${sdpCon.blocks.size}\n")
      psd.sizes.foreach { size => writer.append(s"$size\n") }
      writer.append("\n")
      // G_i = sum_j H_ij x_j + D_i
      writer.append("HCOORD\n")
      for ( HCoordElement(i, j, r, c, real) <- psd.hCoord )
        writer.append(s"$i $j $r $c $real\n")
      writer.append("\n")
      writer.append("DCOORD\n")
      for ( DCoordElement(i, r, c, real) <- psd.dCoord )
        writer.append(s"$i $r $c $real\n")
      writer.append("\n")
    }

  }

}
