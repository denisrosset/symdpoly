package net.alasc.symdpoly
package sdp

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
import net.alasc.symdpoly.solvers
import net.alasc.symdpoly.solvers.{JOptimizerInstance, MosekFormat, SCSMatlabFormat, SDPAFormat, SDPT3MatlabFormat, SedumiMatlabFormat}
import net.alasc.syntax.all._

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
case class Program(direction: Direction, obj: Vec[Double], sdpCon: SDPConstraint, eqA: Mat[Double], ineqA: Mat[Double]) {

  def nY: Int = obj.length

  def asMinimization: Program = direction match {
    case Direction.Minimize => this
    case Direction.Maximize => Program(Direction.Minimize, -obj, sdpCon, eqA, ineqA)
  }

  def convertEqualitiesToInequalities: Program = Program(direction, obj, sdpCon, Mat.zeros[Double](0, obj.length), ineqA vertcat eqA vertcat (-eqA))

  def convertInequalitiesToBlock: Program = {
    val entries = for {
      r <- (0 until ineqA.nRows).toArray
      c <- 0 until ineqA.nCols
      coeff = ineqA(r, c) if coeff != 0
    } yield (c, r, r, coeff)
    val size = ineqA.nRows
    val basisSize = obj.length
    val newBlock = Block(size, basisSize, entries.map(_._1), entries.map(_._2), entries.map(_._3), entries.map(_._4))
    val newRepresentation = RepMat.realTrivialMorphism[Perm](size)
    Program(direction, obj, sdpCon.adding(Seq(newBlock), newRepresentation), eqA, Mat.zeros[Double](0, obj.length))
  }

  def mergeBlocks: Program = Program(direction, obj, sdpCon.mergeBlocks, eqA, ineqA)

  def sdpa: SDPAFormat = SDPAFormat(this)

  def mosek: MosekFormat = MosekFormat(this)

  def scs: SCSMatlabFormat = SCSMatlabFormat(this)

  def sedumi: SedumiMatlabFormat = SedumiMatlabFormat(this)

  def sdpt3: SDPT3MatlabFormat = SDPT3MatlabFormat(this)

  def jOptimizer: JOptimizerInstance = JOptimizerInstance(this)

}
