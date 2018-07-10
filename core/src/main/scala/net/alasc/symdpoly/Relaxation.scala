package net.alasc.symdpoly

import shapeless.Witness
import spire.syntax.action._

import cyclo.Cyclo
import scalin.immutable.Vec
import scalin.immutable.dense._

import net.alasc.finite.Grp
import net.alasc.symdpoly.evaluation.EvaluatedPoly
import net.alasc.symdpoly.math.GenPerm
import net.alasc.symdpoly.solvers._

case class Relaxation[
  E <: evaluation.FreeBasedEvaluator[M, F] with Singleton:Witness.Aux,
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux
](problem: Maximization[E, M, F], generatingSet: GSet[M], symmetryGroup: Grp[GenPerm]) {

  val objective: EvaluatedPoly[E, M, symmetryGroup.type] =
    valueOf[E].apply(problem.evaluatedPoly.normalForm, symmetryGroup)

  val gramMatrix: GramMatrix[M, F] = GramMatrix(generatingSet, valueOf[E], symmetryGroup)

  val objectiveVector: Vec[Cyclo] = objective.vecOverOrderedSet(gramMatrix.momentSet.monomials)

  def isObjectiveReal: Boolean = objectiveVector.toIndexedSeq.forall(c => c.isReal)

  def jOptimizerInstance: JOptimizerInstance = new JOptimizerInstance(this)
  def mosekInstance: MosekInstance = new MosekInstance(this)
  def scsInstance: SCSInstance = new SCSInstance(this)
  def sdpaInstance: SDPAInstance = new SDPAInstance(this)
  def sdpt3Instance: SDPT3Instance = new SDPT3Instance(this)
  def sedumiInstance: SeDuMiInstance = new SeDuMiInstance(this)

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
  def writeMomentIndexMatrix(fileName: String): Unit = {
    import scalin.immutable.dense._

    import better.files._
    import gramMatrix._
    val mat = momentIndexMatrix
    File(fileName).createIfNotExists()
      .clear()
      .appendLine(s"${matrixSize} ${nUniqueMonomials}")
      .appendLines(Seq.tabulate(matrixSize)( r => mat(r, ::).toIndexedSeq.mkString(" ") ):_*)
  }

  /** Writes the sign/phase of the monomials present in the Gram matrix. */
  def writePhaseMatrix(fileName: String): Unit = {
    import scalin.immutable.dense._

    import better.files._
    import gramMatrix._
    val mat = phaseMatrix
    File(fileName).createIfNotExists()
      .clear()
      .appendLine(s"${matrixSize} ${nUniqueMonomials}")
      .appendLines(Seq.tabulate(matrixSize)( r => mat(r, ::).toIndexedSeq.mkString(" ") ):_*)
  }

  def writeMomentMatrix(fileName: String): Unit = {
    import better.files._
    import gramMatrix._
    val matString = scalin.Printer.mat(momentMatrix, Int.MaxValue, Int.MaxValue)
    File(fileName).createIfNotExists()
      .clear()
      .append(matString)
  }

  def writeCanonicalMonomials(fileName: String): Unit = {
    import better.files._
    import gramMatrix._
    val monomials = Vector.tabulate(momentSet.nMonomials) { i =>
      import spire.compat._
      val orbit = symmetryGroup.iterator.map((g: GenPerm) => valueOf[E].apply(momentSet.monomials(i) <|+| g).normalForm)
        .toSet.toVector.sorted.map(_.normalForm).mkString(", ")
      s"${i} ${momentSet.monomials(i).normalForm.normalForm}: ${orbit}"
    }
    File(fileName).createIfNotExists()
      .clear()
      .appendLines(monomials:_*)
  }

  def writeSymmetryGroupDescription(fileName: String): Unit = {
    import better.files._
    import net.alasc.perms.default._
    val generators = symmetryGroup.smallGeneratingSet.map(g => free.Generator.prettyPrintGenPerm(g, valueOf[F]))
    File(fileName).createIfNotExists()
      .clear()
      .appendLine(s"Number of generators: ${generators.length}")
      .appendLine(s"Group order: ${symmetryGroup.order}")
      .appendLines(generators:_*)
  }

}
