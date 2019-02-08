package net.alasc.symdpoly.joptimizer

import scala.annotation.tailrec

import spire.syntax.cfor.cforRange

import scalin.immutable.Vec
import scalin.immutable.dense._

import com.joptimizer.functions.{LinearMultivariateRealFunction, SDPLogarithmicBarrier}
import com.joptimizer.optimizers.{BarrierMethod, OptimizationRequest}
import net.alasc.symdpoly.solvers.{Instance, Instance2}
import net.alasc.symdpoly.{OptimumFound, Relaxation, Relaxation2, Solution}

class JOptimizerInstance(val relaxation: Relaxation[_, _, _]) extends Instance {
  import relaxation.{gramMatrix, objectiveVector}
  import gramMatrix.matrixSize

  val nDualVariables: Int = gramMatrix.nUniqueMonomials - 1

  def Gmatrix: Array[Array[Double]] = {
    val res = Array.tabulate(matrixSize)(x => new Array[Double](matrixSize))
    cforRange(0 until matrixSize) { r =>
      cforRange(0 until matrixSize) { c =>
        if (gramMatrix.momentIndex(r, c) == 0)
          res(r)(c) = -gramMatrix.phase(r, c).toInt.toDouble
      }
    }
    res
  }
  def Amatrix(i: Int): Array[Array[Double]] = {
    val res = Array.tabulate(matrixSize)(x => new Array[Double](matrixSize))
    cforRange(0 until matrixSize) { r =>
      cforRange(0 until matrixSize) { c =>
        if (gramMatrix.momentIndex(r, c) == i + 1)
          res(r)(c) = -gramMatrix.phase(r, c).toInt.toDouble
      }
    }
    res
  }

  lazy val A = Vector.tabulate(nDualVariables)(Amatrix)
  lazy val b = Array.tabulate(nDualVariables)(i => -cycloToDouble(objectiveVector(i + 1)))
  lazy val G = Gmatrix
  lazy val cfix = cycloToDouble(objectiveVector(0))

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

class JOptimizerInstance2(val relaxation: Relaxation2[_, _]) extends Instance2 {
  import relaxation.{gramMatrix, objectiveVector}
  import gramMatrix.matrixSize

  val nDualVariables: Int = gramMatrix.nUniqueMonomials - 1

  def Gmatrix: Array[Array[Double]] = {
    val res = Array.tabulate(matrixSize)(x => new Array[Double](matrixSize))
    cforRange(0 until matrixSize) { r =>
      cforRange(0 until matrixSize) { c =>
        if (gramMatrix.momentIndex(r, c) == 0)
          res(r)(c) = -gramMatrix.phase(r, c).toInt.toDouble
      }
    }
    res
  }
  def Amatrix(i: Int): Array[Array[Double]] = {
    val res = Array.tabulate(matrixSize)(x => new Array[Double](matrixSize))
    cforRange(0 until matrixSize) { r =>
      cforRange(0 until matrixSize) { c =>
        if (gramMatrix.momentIndex(r, c) == i + 1)
          res(r)(c) = -gramMatrix.phase(r, c).toInt.toDouble
      }
    }
    res
  }

  lazy val A = Vector.tabulate(nDualVariables)(Amatrix)
  lazy val b = Array.tabulate(nDualVariables)(i => -cycloToDouble(objectiveVector(i + 1)))
  lazy val G = Gmatrix
  lazy val cfix = cycloToDouble(objectiveVector(0))

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
