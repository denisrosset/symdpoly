package net.alasc.symdpoly

import shapeless.Witness

import cyclo.Cyclo
import scalin.immutable.Vec

import net.alasc.symdpoly.evaluation.{EvaluatedPoly, Evaluator}
import net.alasc.symdpoly.solvers.{MosekInstance, SDPAInstance}
import scalin.immutable.dense._

import net.alasc.finite.Grp
import net.alasc.symdpoly.math.GenPerm

/** Moment/SOS-based relaxation of a polynomial maximization problem.
  * @param problem       Problem to construct the relaxation for
  * @param generatingSet Set of monomials used to generate the moment matrix
  */
case class Relaxation[
  E <: Evaluator[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux,
](problem: Maximization[E, M], generatingSet: GSet[M]) {

  val objective: EvaluatedPoly[E, M] =
    valueOf[E].apply(problem.evaluatedPoly.normalForm)

  val gramMatrix: GramMatrix[E, M] = GramMatrix(valueOf[E], generatingSet)

  val objectiveVector: Vec[Cyclo] = objective.vecOverOrderedSet(gramMatrix.momentSet.elements)

  def isObjectiveReal: Boolean = objectiveVector.toIndexedSeq.forall(c => c.isReal)

  def mosekInstance: MosekInstance = new MosekInstance(this)
  def sdpaInstance: SDPAInstance = new SDPAInstance(this)

  /** Writes the Gram matrix indices to the file with given filename.
    * The structure of the file is as follows
    *
    * Line   1: N M
    * Line   2: g11 g12 ... g1N
    * ...
    * Line 1+N: gN1 gN2 ... gNN
    *
    * where N is the size of the Gram matrix, and M is the number of monomials involved,
    * including the identity.
    *
    * The matrix index g(r,c) takes the value
    *
    * -  0 if the element is always 0
    * -  1 if the element takes the constant value  1 or -1
    * -  j if the element corresponds to the expectation value of j-th monomial (with a possible factor -1)
    *
    * and max_{r c} g(r,c) = M
    *
    */
  def momentIndexMatrixDescription: String = {
    import scalin.immutable.dense._
    import gramMatrix._
    val mat = momentIndexMatrix
    s"${matrixSize} ${nUniqueMonomials}\n" ++ Seq.tabulate(matrixSize)( r => mat(r, ::).toIndexedSeq.mkString(" ") ).mkString("\n")
  }


  /** Writes the sign/phase of the monomials present in the Gram matrix. */
  def phaseMatrixDescription: String = {
    import scalin.immutable.dense._
    import gramMatrix._
    val mat = phaseMatrix
    s"${matrixSize}\n" ++ Seq.tabulate(matrixSize)( r => mat(r, ::).toIndexedSeq.mkString(" ") ).mkString("\n")
  }

  def momentMatrixDescription: String = scalin.Printer.mat(gramMatrix.momentMatrix, Int.MaxValue, Int.MaxValue)

  /*
  def canonicalMonomialsDescription: String = {
    import gramMatrix._
    val monomials = Vector.tabulate(momentSet.nElements) { i =>
      import spire.compat._
      val orbit = symmetryGroup.iterator.map((g: GenPerm) => valueOf[E].apply(momentSet.monomials(i) <|+| g).normalForm)
        .toSet.toVector.sorted.map(_.normalForm).mkString(", ")
      s"${i} ${momentSet.monomials(i).normalForm.normalForm}: ${orbit}"
    }
    monomials.mkString("\n")
  }

  def describeGroup(grp: Grp[GenPerm], printGen: GenPerm => String): String = {
    import net.alasc.perms.default._
    val generators = grp.smallGeneratingSet.map(printGen)
    s"Number of generators: ${generators.length}\n" ++ s"Group order: ${grp.order}\n" ++ generators.mkString("\n")
  }

  def symmetryGroupDescription: String = describeGroup(symmetryGroup, g => generic.FreeBasedPermutation.prettyPrintGenPerm(g, valueOf[F]))

  def matrixSymmetryGroupDescription: String = describeGroup(gramMatrix.matrixSymmetries, _.toString)*/

}
