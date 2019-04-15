package net.alasc

import cyclo.{Cyclo, RealCyclo}

import net.alasc.finite.Grp
import shapeless.Witness
import spire.algebra.{Action, AdditiveGroup}

import scalin.SparseAdditiveGroup
import scala.collection.mutable

import spire.std.double._
import spire.syntax.order._
import spire.math.Complex

import net.alasc.bsgs.GrpChainPermutationAction
import net.alasc.symdpoly.math.GrpDecomposition.Attributes
import net.alasc.symdpoly.math.{GrpDecomposition, Phase}
import net.alasc.symdpoly.util.MemoMap

/** SymDPoly contains different packages.
  *
  * - Monomial monoids and associated polynomial rings are defined in the
  *   [[net.alasc.symdpoly.generic `generic`]], [[net.alasc.symdpoly.free `free`]],
  *   [[net.alasc.symdpoly.freebased `free based`]] and [[net.alasc.symdpoly.quotient `quotient`]]
  *   packages.
  *
  * - The evaluation of monomials into moments is performed through [[net.alasc.symdpoly.evaluation `evaluation`]].
  *
  * - Additional algebraic structures are defined in [[net.alasc.symdpoly.algebra `algebra`]], with syntax in
  *   [[net.alasc.symdpoly.syntax `syntax`]].
  *
  * - The packages [[net.alasc.symdpoly.sdp `sdp`]] and [[net.alasc.symdpoly.solvers `solvers`]] respectively
  *   define floating point representations of conic linear programs and interface the supported solvers.
  *
  * To obtain a good set of default types, values, syntax enrichments and instances,
  * simply import `net.alasc.symdpoly.defaults._` in your code.
  */
package object symdpoly {

  def trivialAction[A]: Action[A, Unit] = new Action[A, Unit] {
    def actl(g: Unit, a: A): A = a
    def actr(a: A, g: Unit): A = a
  }

  implicit def cycloFromInt(i: Int): Cyclo = Cyclo(i)

  implicit val sparseDouble: SparseAdditiveGroup[Double] = new SparseAdditiveGroup[Double] {
    def zero: Double = 0.0
    def provenZero(a: Double): Boolean = a == 0.0
    def additive: AdditiveGroup[Double] = spire.std.double.DoubleAlgebra
  }

  @inline def valueOf[S <: Singleton](implicit wS: Witness.Aux[S]): S = wS.value

  implicit class GrpDecompositionOps[G](val lhs: Grp[G]) extends AnyVal {
    def decomposition(implicit G: GrpChainPermutationAction[G]): GrpDecomposition[G] =
      Attributes.Decompostion(lhs)(GrpDecomposition.make[G](lhs))
  }

  implicit class RichRealCyclo(val rc: RealCyclo) extends AnyVal {
    def toDouble: Double =
      if (rc.isRational) rc.toRational.toDouble else rc.toAlgebraic.toDouble
  }

  /** Cached floating point approximations of cyclotomics. */
  val cycloValue: MemoMap[Cyclo, Complex[Double]] = MemoMap(c => Complex(RealCyclo.real(c).toDouble, RealCyclo.imag(c).toDouble))

  /** Cached floating point approximations of phases. */
  val phaseValue: MemoMap[Phase, Complex[Double]] = MemoMap(p => cycloValue(p.toCyclo))

  /** Logs the given message if the current verbosity level is >= than the given verbosity level. */
  def log(level: Verbosity)(text: => String): Unit = if (level >= Settings.verbosity) Settings.err.println(text)

  /** Logs the given message if the current verbosity level is >= Verbosity.Normal.
    *
    * Use for coarse updates about the progress of the computation.
    */
  def logNormal(text: => String): Unit = log(Verbosity.Normal)(text)

  /** Logs the given message if the current verbosity level is >= Verbosity.Verbose. */
  def logVerbose(text: => String): Unit = log(Verbosity.Verbose)(text)

  /** Computes the given function while setting the Settings.optimize flag to true.
    *
    * When possible, optimized algorithms will be used.
    */
  def optimized[T](thunk: => T): T = Settings.withOptimize(true)(thunk)

  /** Computes the given function while setting the Settings.optimize flag to false.
    *
    * Disables some optimizations by running the boring/slow/robust code path. Useful to debug. */
  def unoptimized[T](thunk: => T): T = Settings.withOptimize(false)(thunk)

  /** Runs the given function twice, once with optimization on, once with optimization off and compares the results. */
  def checkOptimize[T](thunk: => T): T = {
    val optimizedT = optimized(thunk)
    val unoptimizedT = unoptimized(thunk)
    assert(optimizedT == unoptimizedT, s"The values $optimizedT and $unoptimizedT should be equal")
    optimizedT
  }

}
