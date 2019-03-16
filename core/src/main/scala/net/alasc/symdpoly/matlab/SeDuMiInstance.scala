package net.alasc.symdpoly
package matlab

import spire.math.Complex
import spire.syntax.cfor._

import com.jmatio.io.MatFileWriter
import com.jmatio.types._
import cyclo.Cyclo

import net.alasc.finite.FaithfulPermutationActionBuilder
import net.alasc.perms.Perm
import net.alasc.symdpoly.math.GenPerm
import net.alasc.syntax.all._
import scalin.immutable.{Mat, Vec}
import scalin.immutable.csc._
import spire.std.double._
import spire.std.int._

import net.alasc.algebra.PermutationAction
import syntax.phased._
import scalin.syntax.all._


/*
/** Export interface for the Sedumi solver
  *
  * We write the problem as
  *
  * maximize b * y
  *
  * A' y + s = c
  * s in K*
  *
  * => c - A' y in K*
  *
  *
  *
  * @param sdp
  */
case class SedumiInstance(val sdp: SDP) extends MatlabFormat {

  def convertBlock(block: SDP.Block): (Mat[Double], Vec[Double]) = {
    import scalin.immutable.csc._
    val n = block.size

    def index(r: Int, c: Int): Int = c * n + r // col major storage
    val datab = for {
      i <- 0 until block.nEntries
      j = block.basisIndices(i) if j == 0
      r = block.rowIndices(i)
      c = block.colIndices(i)
      e = block.coefficients(i)
    } yield (index(r, c), e)
    val dataA = for {
      i <- 0 until block.nEntries
      j = block.basisIndices(i) if j > 0
      r = block.rowIndices(i)
      c = block.colIndices(i)
      e = block.coefficients(i)
    } yield (j - 1, index(r, c), -e) // note the transpose here, basis index is the row index
    // and there is a sign change
    val matA = Mat.sparse[Double](n * n, block.basisSize - 1)(Vec(dataA.map(_._1): _*), Vec(dataA.map(_._2): _*), Vec(dataA.map(_._3): _*))
    val vecA = Vec.fromMutable(n * n, 0.0) { mut =>
      for ((i, e) <- datab) mut(i) := e
    }
    (matA, vecA)
  }

  def data: Struct = {
    val k = Struct(
      "f" -> Scalar(sdp.eqA.nRows), "l" -> Scalar(sdp.ineqA.nRows),
      "q" -> Vect.emptyRow, "r" -> Vect.emptyRow,
      "s" -> Vect.row(sdp.blocks.map(_.size.toDouble)))
    val eqb = sdp.eqA(::, 0)
    val ineqb = sdp.ineqA(::, 0)
    val eqA = -sdp.eqA(::, 1 until sdp.eqA.nCols).t
    val ineqA = -sdp.ineqA(::, 1 until sdp.ineqA.nCols).t
    val (blocksA, blocksb) = sdp.blocks.map(convertBlock).unzip
    val matA = Matrix(Seq[Mat[Double]](Seq(eqA, ineqA) ++ blocksA: _*).reduce(_ horzcat _))
    val vecb = Vect.col(Seq[Vec[Double]](Seq(eqb, ineqb) ++ blocksb: _*).reduce(_ cat _))
    val sign = sdp.direction match {
      case Direction.Minimize => -1.0
      case Direction.Maximize => 1.0
    }
    val c = Vect.col(sdp.obj(1 until sdp.obj.length) * sign)
    sdp.blocks match {
      case Seq(block) =>
        val
      case _ =>
        Struct("K" -> k, "A" -> matA, "b" -> vecb, "c" -> c, "objShift" -> Scalar(sdp.obj(0)), "objFactor" -> Scalar(sign))
    }
    val fullSym = MatrixSymmetry.directSumMonoid.combineAll(sdp.blocks.map(_.symmetry)).onPermutationGroup
    val permGeneratorsLeftAction = fullSym.grp.generators.map(_.inverse)

    val g = CellArray(Mat.rowMajor(1, fullSym.grp.nGenerators)(fullSym.grp.generators.map(_.matlabImage(fullSym.n): MatlabData): _*))
    val rho = CellArray(Mat.rowMajor(1, fullSym.grp.nGenerators)(Matrix(fullSym.generatorImages.map(genPermToMatrix(_, fullSym.n, )
  }

}
*/
/*
class SeDuMiInstance(val relaxation: Relaxation[_, _]) extends Instance {
  import SeDuMiInstance.{SparseMatrix, SparseVector}
  import relaxation.{gramMatrix, objectiveVector}
  import gramMatrix.matrixSize

  // TODO: verify that it corresponds to the published description of GenPerm action

  // TODO require(gramMatrix.momentSet(0).isOne, "Error: empty/one monomial not part of the relaxation")
  val (nG, gperms, rho) = {
    import gramMatrix.matrixSymmetries
    val nG = matrixSymmetries.grp.nGenerators
    val perms = matrixSymmetries.grp.generators.map(matrixSymmetries.niceMorphism)
    val genPerms = matrixSymmetries.grp.generators.map(matrixSymmetries.representation)
    val domainSize = PermutationAction[Perm].largestMovedPoint(perms).fold(1)(_ + 1)
    val G: Seq[Array[Double]] = perms.map(_.images(domainSize).map(i => (i + 1).toDouble).toArray)
    val rho: Seq[SparseMatrix] = genPerms.map(g => genPermToSparseMatrix(g, matrixSize))
    (nG, G, rho)
  }

  val Gcell: MLCell = new MLCell("G", Array(1, nG))
  gperms.zipWithIndex.foreach { case (g, i) => Gcell.set(new MLDouble(null, g, 1), i) }

  val rhoCell: MLCell = new MLCell("rho", Array(1, nG))
  rho.zipWithIndex.foreach { case (r, i) => rhoCell.set(r.toMATLAB(null), i) }

  val m: Int = gramMatrix.nUniqueMonomials - 1 // number of dual variables
  val n: Int = matrixSize * matrixSize

  case class K(f: Int, l: Int, q: Array[Int], r: Array[Int], s: Array[Int])

  val k = K(0, 0, Array(), Array(), Array(matrixSize))

  val c = SparseVector.forMoment(gramMatrix, 0)

  def aMatrix: SparseMatrix = {
    val columns = Array.tabulate(m)(c => SparseVector.forMoment(gramMatrix, c + 1, -1.0))
    val rows = columns.flatMap(_.indices)
    val cols = columns.zipWithIndex.flatMap { case (col, c) => Array.fill(col.nEntries)(c) }
    val data = columns.flatMap(_.data)
    SparseMatrix(rows, cols, data, n, m)
  }

  val a = aMatrix

  val b = Array.tabulate(m)(i => realCycloToDouble(objectiveVector(i + 1)))

  val objShift = realCycloToDouble(objectiveVector(0)) // constant in objective not supported

  def writeFile(fileName: String): Unit = {
    val file = new java.io.File(fileName)
    val dataK = new MLStructure("K", Array(1, 1))
    dataK.setField("f", new MLDouble(null, Array(k.f.toDouble), 1))
    dataK.setField("l", new MLDouble(null, Array(k.l.toDouble), 1))
    dataK.setField("q", new MLDouble(null, k.q.map(_.toDouble), 1))
    dataK.setField("r", new MLDouble(null, k.r.map(_.toDouble), 1))
    dataK.setField("s", new MLDouble(null, k.s.map(_.toDouble), 1))
    val dataB = new MLDouble("b", b, m)
    val dataC = new MLSparse("c", Array(c.length, 1), 0, c.nEntries)
    cforRange(0 until c.nEntries) { i =>
      dataC.set(c.data(i), c.indices(i), 0)
    }
    val dataA = new MLSparse("A", Array(n, m), 0, a.nEntries)
    cforRange(0 until a.nEntries) { i =>
      dataA.set(a.data(i), a.rows(i), a.cols(i))
    }
    val dataObjShift = new MLDouble("objShift", Array(objShift), 1)
    val list = new java.util.ArrayList[MLArray]()
    list.add(dataK)
    list.add(dataB)
    list.add(dataC)
    list.add(dataA)
    list.add(dataObjShift)
    list.add(Gcell)
    list.add(rhoCell)

    new MatFileWriter(file, list)
  }

}

object SeDuMiInstance {

  case class SparseVector(indices: Array[Int], data: Array[Double], length: Int) {
    def nEntries: Int = indices.length
  }

  object SparseVector {
    def forMoment(gramMatrix: GramMatrix[_, _], momentIndex: Int, factor: Double = 1.0): SparseVector = {
      import gramMatrix.matrixSize
      val indices = metal.mutable.Buffer.empty[Int]
      val data = metal.mutable.Buffer.empty[Double]
      cforRange(0 until matrixSize) { c =>
        cforRange(0 until matrixSize) { r =>
          // matlab has row-major indexing (not that it counts for symmetric matrices...)
          val index = r + c * matrixSize
          if (gramMatrix.momentIndex(r, c) == momentIndex) {
            indices += index
            data += gramMatrix.phase(r, c).toInt.toDouble * factor
          }
        }
      }
      SparseVector(indices.toArray, data.toArray, matrixSize * matrixSize)
    }
  }

  case class SparseMatrix(rows: Array[Int], cols: Array[Int], data: Array[Double], nRows: Int, nCols: Int) {
    def nEntries: Int = rows.length
    override def toString:String = s"SparseMatrix(${rows.toSeq}, ${cols.toSeq}, ${data.toSeq}, $nRows, $nCols)"
    def toMATLAB(name: String): MLSparse = {
      val res = new MLSparse(name, Array(nRows, nCols), 0, nEntries)
      cforRange(0 until nEntries) { i =>
        res.set(data(i), rows(i), cols(i))
      }
      res
    }

  }

}
*/
