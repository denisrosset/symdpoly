package net.alasc.symdpoly
package joptimizer

import java.io.Writer

import scala.annotation.tailrec

import spire.syntax.cfor.cforRange

import scalin.immutable.Vec
import scalin.immutable.dense._

import com.joptimizer.functions.{LinearMultivariateRealFunction, SDPLogarithmicBarrier}
import com.joptimizer.optimizers.{BarrierMethod, OptimizationRequest}
import net.alasc.symdpoly.solvers.OldInstance
import net.alasc.symdpoly.{OldRelaxation, OptimumFound, Solution}

case class JOptimizerInstance(val sdp: SDP) {
  import sdp._

  val m: Int = objToMaximize.length - 1

  def solve(tol: Double = 1e-9): Solution =
    if (eqA.nRows > 0) JOptimizerInstance(sdp.convertEqualitiesToInequalities).solve(tol)
    else if (ineqA.nRows > 0) JOptimizerInstance(sdp.convertInequalitiesToBlock).solve(tol)
    else if (blocks.size > 1) JOptimizerInstance(sdp.mergeBlocks).solve(tol)
    else {
      import scala.collection.JavaConverters._
      val block = blocks(0)
      val matrices = Array.fill(objToMaximize.length, block.size, block.size)(0.0)
      cforRange(0 until block.nEntries) { i =>
        val di = block.basisIndex(i)
        val r = block.rowIndex(i)
        val c = block.colIndex(i)
        val e = -block.coeffs(i)
        matrices(di)(r)(c) = e
      }
      val cfix = objToMaximize(0)
      val b = Array.tabulate(m)(i => -objToMaximize(i + 1))
      val G = matrices(0)
      val A = matrices.tail.toVector
      import scala.collection.JavaConverters._
      val obj = new LinearMultivariateRealFunction(b, cfix)
      val or = new OptimizationRequest
      or.setTolerance(tol)
      or.setF0(obj)
      or.setInitialPoint(new Array[Double](A.length))
      val bf = new SDPLogarithmicBarrier(A.asJava, G)
      val opt = new BarrierMethod(bf)
      opt.setOptimizationRequest(or)
      opt.optimize()
      val jOptimizerSolution = opt.getOptimizationResponse.getSolution
      val sol = Vec.tabulate(m + 1) {
        case 0 => 1.0
        case i => jOptimizerSolution(i - 1)
      }
      @tailrec def iter(i: Int, acc: Double): Double =
        if (i == m + 1) acc else iter(i + 1, acc + sol(i) * objToMaximize(i))
      val value = iter(0, 0.0)
      OptimumFound(None, value, None, sol)
    }
}

class OldJOptimizerInstance(val relaxation: OldRelaxation[_, _]) extends OldInstance {
  import relaxation.{momentMatrix, objectiveVector}
  import momentMatrix.matrixSize

  val nDualVariables: Int = momentMatrix.nUniqueMonomials - 1

  def Gmatrix: Array[Array[Double]] = {
    val res = Array.tabulate(matrixSize)(x => new Array[Double](matrixSize))
    cforRange(0 until matrixSize) { r =>
      cforRange(0 until matrixSize) { c =>
        if (momentMatrix.momentIndex(r, c) == 0)
          res(r)(c) = -momentMatrix.phase(r, c).toInt.toDouble
      }
    }
    res
  }

  def Amatrix(i: Int): Array[Array[Double]] = {
    val res = Array.tabulate(matrixSize)(x => new Array[Double](matrixSize))
    cforRange(0 until matrixSize) { r =>
      cforRange(0 until matrixSize) { c =>
        if (momentMatrix.momentIndex(r, c) == i + 1)
          res(r)(c) = -momentMatrix.phase(r, c).toInt.toDouble
      }
    }
    res
  }

  lazy val A = Vector.tabulate(nDualVariables)(Amatrix)
  lazy val b = Array.tabulate(nDualVariables)(i => -realCycloToDouble(objectiveVector(i + 1)))
  lazy val G = Gmatrix
  lazy val cfix = realCycloToDouble(objectiveVector(0))

  def solve(tol: Double = 1e-9): Solution = {
    import scala.collection.JavaConverters._
    val obj = new LinearMultivariateRealFunction(b, cfix)
    val or = new OptimizationRequest
    or.setTolerance(tol)
    or.setF0(obj)
    or.setInitialPoint(new Array[Double](A.length))
    val bf = new SDPLogarithmicBarrier(A.asJava, G)
    val opt = new BarrierMethod(bf)
    opt.setOptimizationRequest(or)
    opt.optimize()
    val jOptimizerSolution = opt.getOptimizationResponse.getSolution
    val sol = Vec.tabulate(nDualVariables + 1) {
      case 0 => 1
      case i => -jOptimizerSolution(i - 1)
    }
    @tailrec def iter(i: Int, acc: Double): Double =
      if (i == nDualVariables) acc else iter(i + 1, acc - jOptimizerSolution(i) * b(i))
    val value = iter(0, cfix)
    OptimumFound(None, value, None, sol)
  }
}
