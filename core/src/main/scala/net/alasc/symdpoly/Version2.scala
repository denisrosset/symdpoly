package net.alasc.symdpoly

import shapeless.Witness
import spire.algebra.{Action, Group, Involution}
import spire.syntax.cfor.cforRange

import cyclo.Cyclo
import scalin.immutable.{Mat, Vec}

import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.symdpoly
import net.alasc.symdpoly.algebra.{Instances, Morphism, MultiplicativeBinoid}
import net.alasc.symdpoly.evaluation._
import net.alasc.symdpoly.internal.{MomentSet, MomentSet2, MomentSetBuilder, MomentSetBuilder2}
import net.alasc.symdpoly.math.{GenPerm, PhasedInt, Phases}
import net.alasc.symdpoly.solvers.{MosekInstance, SDPAInstance}
import net.alasc.util.Tuple2Int
import spire.syntax.multiplicativeMonoid._
import spire.syntax.involution._
import spire.std.unit._
import spire.syntax.action._

import net.alasc.symdpoly.algebra.Phased.syntax._
import scalin.immutable.dense._
import spire.std.int._

case class Maximization2[
  E <: Evaluator2[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton: Witness.Aux
](evaluatedPoly: EvaluatedPoly2[E, M]) {

  def E: E = valueOf[E]
  def M: M = valueOf[M]

  //def relaxation(generatingSet: GSet[M]): Relaxation2[E, M, Unit] =
  //  Relaxation2(this, generatingSet, Grp.trivial[Unit], Instances.trivialAction[M#Monomial])

  /*
  /** Constructs a symmetric relaxation from the subgroup of the ambient group that preserves
    * the objective (thus computes that subgroup).
    *
    * Note that the ambient group should be compatible with both the quotient monoid
    * and the evaluation function.
    *
    */
  def symmetricRelaxation(generatingSet: GSet[M], ambientGroup: Grp[GenPerm]): Relaxation[E, M, F] =
    Relaxation(this, generatingSet, Symmetries.symmetrySubgroup(evaluatedPoly, ambientGroup))

  def symmetricRelaxation(generatingSet: GSet[M]): Relaxation[E, M, F] = {
    // TODO: prove that it is enough
    val ambientGroup = M.symmetryGroup(cyclotomicOrder(evaluatedPoly.normalForm))
    symmetricRelaxation(generatingSet, ambientGroup)
  }

  /** Constructs a symmetric relaxation forcing symmetry by the given group. */
  def forcedSymmetricRelaxation(generatingSet: GSet[M], group: Grp[GenPerm]): Relaxation[E, M, F] =
    Relaxation(this, generatingSet, group)*/

  // def cyclotomicOrder(poly: GenPoly[_]): Int = (0 until poly.nTerms).map(i => poly.coeff(i).order).foldLeft(2)(spire.math.lcm[Int](_, _))

}

case class Relaxation2[
  E <: Evaluator2[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux,
](problem: Maximization2[E, M], generatingSet: GSet[M]) {

  val objective: EvaluatedPoly2[E, M] =
    valueOf[E].apply(problem.evaluatedPoly.normalForm)

/*  val gramMatrix: GramMatrix2[E, M] = GramMatrix2(generatingSet, valueOf[E])

  val objectiveVector: Vec[Cyclo] = objective.vecOverOrderedSet(gramMatrix.momentSet.monomials)

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

  def canonicalMonomialsDescription: String = {
    import gramMatrix._
    val monomials = Vector.tabulate(momentSet.nMonomials) { i =>
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

  def symmetryGroupDescription: String = describeGroup(symmetryGroup, g => free.OpGenPerm.prettyPrintGenPerm(g, valueOf[F]))

  def matrixSymmetryGroupDescription: String = describeGroup(gramMatrix.matrixSymmetries, _.toString)
*/
}

class GramMatrix2[
  E <: Evaluator2[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux
](val generatingMoments: OrderedSet[M#Monomial],
  val momentSet: MomentSet2[E, M],
  private[this] val momentArray: Array[Int],
  private[this] val phaseArray: Array[Int] // phase encoding
 ) {
  def E: E = valueOf[E]
  def M: M = valueOf[M]

  def isReal: Boolean = {
    cforRange(0 until phaseArray.length) { i =>
      if (phaseArray(i) != Phase.one.encoding && phaseArray(i) != Phase.minusOne.encoding)
        return false
    }
    momentSet.allSelfAdjoint
  }

  val matrixSize = generatingMoments.length

  private[this] def inMat(r: Int, c: Int): Int = r + c * matrixSize

  scalin.immutable.dense.matEngine[Int]

  def momentIndexMatrix: Mat[Int] = Mat.tabulate(matrixSize, matrixSize) { (r, c) => momentIndex(r, c)}
  def phaseMatrix: Mat[Phase] = Mat.tabulate(matrixSize, matrixSize) { (r, c) => phase(r, c) }

  def momentIndex(r: Int, c: Int): Int = momentArray(inMat(r, c))

  def absMoment(r: Int, c: Int): EvaluatedMono2[E, M] = momentIndex(r, c) match {
    case -1 => E.evaluatedMonoZero
    case i => momentSet(i)
  }

  def moment(r: Int, c: Int): EvaluatedMono2[E, M] = absMoment(r, c) <* phase(r, c)
  def phase(r: Int, c: Int): Phase = Phase.fromEncoding(phaseArray(inMat(r, c)))
  def nUniqueMonomials: Int = momentSet.nElements

  def momentMatrix: Mat[EvaluatedMono2[E, M]] = Mat.tabulate(matrixSize, matrixSize) { (r, c) => moment(r, c) }

}

object GramMatrix2 {

  def apply[
    E <: Evaluator2[M] with Singleton: Witness.Aux,
    M <: generic.MonoidDef with Singleton: Witness.Aux
  ](gSet: GSet[M]): GramMatrix2[E, M] = {
    def E: E = valueOf[E]
    def M: M = valueOf[M]
    val generatingMoments = OrderedSet.fromOrdered(gSet.monomials.toVector)
    val n = generatingMoments.length
    def inMat(r: Int, c: Int): Int = r + c * n
    val phaseMatrix = Array.fill[Int](n * n)(Phase.one.encoding)
    val unsortedMomentMatrix = Array.fill[Int](n * n)(Int.MinValue)
    val sb = MomentSetBuilder2.make[E, M]
    cforRange(0 until n) { r =>
      cforRange(r until n) { c =>
        if (unsortedMomentMatrix(inMat(r, c)) == Int.MinValue) {
          implicit def involution: Involution[M#Monomial] = M.monoInvolution
          implicit def monoMultiplicativeBinoid: MultiplicativeBinoid[M#Monomial] = M.monoMultiplicativeBinoid
          val phased: EvaluatedMono2[E, M] = E(generatingMoments(r).adjoint * generatingMoments(c))
          val phase = phased.phaseOffset
          val canonical = phased.phaseCanonical
          // TODO: check phase support when complex is supported
          if (phased.isZero) {
            unsortedMomentMatrix(inMat(r, c)) = -1
            unsortedMomentMatrix(inMat(c, r)) = -1
          } else if (r == c || E.isSelfAdjoint) {
            val index = sb.getElement(canonical)
            unsortedMomentMatrix(inMat(r, c)) = index
            unsortedMomentMatrix(inMat(c, r)) = index
            phaseMatrix(inMat(r, c)) = phase.encoding
            phaseMatrix(inMat(c, r)) = phase.encoding
          } else {
            val phasedAdj = E(generatingMoments(c).adjoint * generatingMoments(r))
            val phaseAdj = phasedAdj.phaseOffset
            val canonicalAdj = phasedAdj.phaseCanonical
            val tuple = sb.getElement(canonical, canonicalAdj)
            unsortedMomentMatrix(inMat(r, c)) = tuple._1
            unsortedMomentMatrix(inMat(c, r)) = tuple._2
            phaseMatrix(inMat(r, c)) = phase.encoding
            phaseMatrix(inMat(c, r)) = phaseAdj.encoding
          }
        }
      }
    }
    val (sortedMoments, unsortedToSorted) = sb.result()
    val sortedMomentMatrix = unsortedMomentMatrix.map {
      case -1 => -1
      case i => unsortedToSorted.image(i)
    }
    new GramMatrix2[E, M](generatingMoments, sortedMoments, sortedMomentMatrix, phaseMatrix)
  }
}
