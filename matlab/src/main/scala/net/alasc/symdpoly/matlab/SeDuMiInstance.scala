package net.alasc.symdpoly.matlab

import spire.syntax.cfor.cforRange

import com.jmatio.io.MatFileWriter
import com.jmatio.types._
import cyclo.Cyclo

import net.alasc.finite.FaithfulPermutationActionBuilder
import net.alasc.perms.Perm
import net.alasc.symdpoly.math.GenPerm
import net.alasc.symdpoly.solvers.{Instance}
import net.alasc.symdpoly.{ GramMatrix, Relaxation}
import net.alasc.syntax.all._
import scalin.immutable.{Mat, Vec}

import net.alasc.algebra.PermutationAction
import net.alasc.symdpoly.algebra.Phased.syntax._

class SeDuMiInstance(val relaxation: Relaxation[_, _]) extends Instance {
  import SeDuMiInstance.{SparseMatrix, SparseVector}
  import relaxation.{gramMatrix, objectiveVector}
  import gramMatrix.matrixSize

  // TODO: verify that it corresponds to the published description of GenPerm action
  def genPermToSparseMatrix(genPerm: GenPerm, n: Int): SparseMatrix = {
    import genPerm.{perm, phases}
    require(phases.commonRootOrder <= 2)
    val rows: Array[Int] = perm.images(n).toArray
    val cols: Array[Int] = (0 until n).toArray
    val data: Array[Double] = Array.tabulate(n)(i => phases.phaseFor(perm.image(i)).toCyclo.toRational.toDouble)
    SparseMatrix(rows, cols, data, n, n)
  }

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

  val b = Array.tabulate(m)(i => cycloToDouble(objectiveVector(i + 1)))

  val objShift = cycloToDouble(objectiveVector(0)) // constant in objective not supported

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
