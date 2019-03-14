package net.alasc.symdpoly
package joptimizer

import java.io.Writer

import scala.annotation.tailrec

import spire.syntax.cfor.cforRange

import scalin.immutable.Vec
import scalin.immutable.dense._

import com.joptimizer.functions.{LinearMultivariateRealFunction, SDPLogarithmicBarrier}
import com.joptimizer.optimizers.{BarrierMethod, OptimizationRequest}
import net.alasc.symdpoly.{OptimumFound, Solution}

case class JOptimizerInstance(val sdp: SDP) {
  import sdp._

  val m: Int = obj.length - 1

  def solve(tol: Double = 1e-9): Solution =
    if (eqA.nRows > 0) JOptimizerInstance(sdp.convertEqualitiesToInequalities).solve(tol)
    else if (ineqA.nRows > 0) JOptimizerInstance(sdp.convertInequalitiesToBlock).solve(tol)
    else if (blocks.size > 1) JOptimizerInstance(sdp.mergeBlocks).solve(tol)
    else {
      import scala.collection.JavaConverters._
      val block = blocks(0)
      val matrices = Array.fill(obj.length, block.size, block.size)(0.0)
      cforRange(0 until block.nEntries) { i =>
        val di = block.basisIndex(i)
        val r = block.rowIndex(i)
        val c = block.colIndex(i)
        val e = -block.coeffs(i)
        matrices(di)(r)(c) = e
      }
      val sgn = if (direction == Direction.Maximize) -1.0 else 1.0
      val cfix = sgn * obj(0)
      val b = Array.tabulate(m)(i => sgn * obj(i + 1))
      val G = matrices(0)
      val A = matrices.tail.toVector
      import scala.collection.JavaConverters._
      val jobj = new LinearMultivariateRealFunction(b, cfix)
      val or = new OptimizationRequest
      or.setTolerance(tol)
      or.setF0(jobj)
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
        if (i == m + 1) acc else iter(i + 1, acc + sol(i) * obj(i))
      val value = iter(0, 0.0) * sgn
      OptimumFound(None, value) // sol
    }
}
