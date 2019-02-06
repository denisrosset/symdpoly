package net.alasc.symdpoly

import shapeless.Witness
import spire.algebra.{Action, Group}
import spire.syntax.cfor.cforRange

import cyclo.Cyclo
import scalin.immutable.{Mat, Vec}

import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.symdpoly
import net.alasc.symdpoly.algebra.{Instances, Morphism}
import net.alasc.symdpoly.evaluation.{EvaluatedPoly, EvaluatedPoly2, FreeBasedEvaluator}
import net.alasc.symdpoly.internal.{MomentSet, MomentSetBuilder}
import net.alasc.symdpoly.math.{GenPerm, PhasedInt, Phases}
import net.alasc.symdpoly.solvers.{MosekInstance, SDPAInstance}
import net.alasc.util.Tuple2Int
import spire.std.int._
import spire.std.unit._

case class Maximization2[
  E <: evaluation.Evaluator2[M] with Singleton: Witness.Aux,
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

  def cyclotomicOrder(poly: GenPoly[_]): Int = (0 until poly.nTerms).map(i => poly.coeff(i).order).foldLeft(2)(spire.math.lcm[Int](_, _))

}

case class SymmetricRelaxation2[
  E <: evaluation.Evaluator2[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux,
  G
](problem: Maximization2[E, M], generatingSet: GSet[M], symmetryGroup: Grp[G], action: Action[M#Monomial, G]) {

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

/*
class GramMatrix2[
  E <: evaluation.Evaluator2[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux
](val generatingMoments: OrderedSet[M#Monomial],
  val momentSet: MomentSet2[M, F],
  val operatorSymmetries: Grp[GenPerm],
  val matrixSymmetries: Grp[GenPerm],
  val homomorphism: Morphism[GenPerm, GenPerm, Group],
  private[this] val momentArray: Array[Int],
  private[this] val phaseArray: Array[Int] // phase encoding
 )(implicit wM: Witness.Aux[M]) {

  def isReal: Boolean = {
    cforRange(0 until phaseArray.length) { i =>
      if (phaseArray(i) != Phase.one.encoding && phaseArray(i) != Phase.minusOne.encoding)
        return false
    }
    momentSet.allSelfAdjoint
  }

  def M: M = wM.value
  implicit def wF: Witness.Aux[F] = (M.Free: F).witness

  val matrixSize = generatingMoments.length

  private[this] def inMat(r: Int, c: Int): Int = r + c * matrixSize

  def momentIndexMatrix: Mat[Int] = Mat.tabulate(matrixSize, matrixSize) { (r, c) => momentIndex(r, c)}
  def phaseMatrix: Mat[Phase] = Mat.tabulate(matrixSize, matrixSize) { (r, c) => phase(r, c) }

  def momentIndex(r: Int, c: Int): Int = momentArray(inMat(r, c))
  def absMoment(r: Int, c: Int): Mono[M, F] = momentIndex(r, c) match {
    case -1 => symdpoly.Mono.zero[M, F]
    case i => momentSet(i)
  }
  def moment(r: Int, c: Int): Mono[M, F] = absMoment(r, c) * phase(r, c)
  def phase(r: Int, c: Int): Phase = Phase.fromEncoding(phaseArray(inMat(r, c)))
  def nUniqueMonomials: Int = momentSet.nMonomials

  def momentMatrix: Mat[Mono[M, F]] = Mat.tabulate(matrixSize, matrixSize) { (r, c) => moment(r, c) }

}

object GramMatrix {

  def momentSetAction[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
  ](monomials: OrderedSet[Mono[M, F]], g: GenPerm)
   (implicit w: Witness.Aux[M], a: Action[Mono[M, F], GenPerm]): GenPerm = {
    import scala.collection.mutable.{HashMap => MMap}
    val phaseMap: MMap[Int, Phase] = MMap.empty[Int, Phase]
    val n = monomials.length
    val permImages = new Array[Int](n)
    cforRange(0 until n) { i =>
      val image = monomials(i) <|+| g
      val canonical = image.phaseCanonical
      val phase = image.phaseOffset
      val permImage = monomials.indexOf(canonical)
      permImages(i) = permImage
      phaseMap(permImage) = phase
    }
    val perm = Perm.fromImages(permImages)
    val phases = Phases(phaseMap.toVector: _*)
    GenPerm(perm, phases)
  }

  def apply[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton,
  ](gSet: GSet[M], evaluator: FreeBasedEvaluator[M, F], operatorSymmetries: Grp[GenPerm] = Grp.trivial[GenPerm])(implicit wM: Witness.Aux[M]): GramMatrix[M, F] = {

    def M: M = wM.value
    implicit def wF: Witness.Aux[F] = (M.Free: F).witness
    val generatingMoments = OrderedSet.fromOrdered(gSet.monomials.toVector)
    val n = generatingMoments.length
    val generatingMomentsAdjoint = Array.tabulate(n)(i => generatingMoments(i).adjoint)
    val maxDegree = Iterator.range(0, generatingMoments.length).map(i => generatingMoments(i).data.length).max
    val sb = MomentSetBuilder.make[F]
    val homomorphism: Morphism[GenPerm, GenPerm, Group] = Morphism((g: GenPerm) => momentSetAction(generatingMoments, g))
    val matrixSymmetries = homomorphism.grpImage(operatorSymmetries)
    val monoPerms = matrixSymmetries.iterator.toVector
    val groupElements = operatorSymmetries.iterator.toArray

    val phaseMatrix = Array.fill[Int](n * n)(Phase.one.encoding)
    val unsortedMomentMatrix = Array.fill[Int](n * n)(Int.MinValue)

    val pad = evaluator.makeScratchPad
    val scratchMono = free.MutableWord.empty[F](maxDegree * 2)
    val scratchAdjoint = free.MutableWord.empty[F](maxDegree * 2)

    def inMat(r: Int, c: Int): Int = r + c * n

    cforRange(0 until n) { r =>
      cforRange(r until n) { c =>
        if (unsortedMomentMatrix(inMat(r, c)) == Int.MinValue) {
          scratchMono.setToContentOf(generatingMomentsAdjoint(r).data)
          scratchMono *= generatingMoments(c).data

          evaluator.reduceInScratchPad(scratchMono, groupElements, pad) // result in scratchMono

          if (evaluator.isSelfAdjoint) {
            scratchAdjoint.setToContentOf(scratchMono)
          } else {
            scratchAdjoint.setToContentOf(generatingMomentsAdjoint(c).data)
            scratchAdjoint *= generatingMoments(r).data
            evaluator.reduceInScratchPad(scratchAdjoint, groupElements, pad)
          }

          val phase = if (scratchMono.isZero) {
            assert(scratchAdjoint.isZero)
            Phase.one
          } else {
            assert(scratchMono.phase.encoding == scratchAdjoint.phase.adjoint.encoding)
            scratchMono.phase
          }
          val tuple = if (scratchMono.isZero) Tuple2Int(-1, -1)
          else if (scratchMono.compareTo(scratchAdjoint) == 0) { // self-adjoint
            scratchMono.setPhase(Phase.one)
            val i = sb.getElement(scratchMono)
            Tuple2Int(i, i)
          }
          else { // not self-adjoint
            scratchMono.setPhase(Phase.one)
            scratchAdjoint.setPhase(Phase.one)
            sb.getElement(scratchMono, scratchAdjoint)
          }
          val indexMono = tuple._1
          val indexAdjoint = tuple._2
          cforRange(0 until monoPerms.size) { i =>
            val rImage = monoPerms(i).image(PhasedInt(Phase.one, r))
            val cImage = monoPerms(i).image(PhasedInt(Phase.one, c))
            val ri = rImage.index
            val ci = cImage.index
            val rp = rImage.phase
            val cp = cImage.phase
            if (unsortedMomentMatrix(inMat(ri, ci)) == Int.MinValue) {
              if (indexMono == -1) { // zero
                unsortedMomentMatrix(inMat(ri, ci)) = -1
                unsortedMomentMatrix(inMat(ci, ri)) = -1
              } else {
                val finalPhase = rp.adjoint * cp * phase
                unsortedMomentMatrix(inMat(ri, ci)) = indexMono
                unsortedMomentMatrix(inMat(ci, ri)) = indexAdjoint
                phaseMatrix(inMat(ri, ci)) = finalPhase.encoding
                phaseMatrix(inMat(ci, ri)) = finalPhase.adjoint.encoding
              }
            }
          }
        }
      }
    }
    val (sortedMoments, unsortedToSorted) = sb.result[M]()
    val sortedMomentMatrix = unsortedMomentMatrix.map {
      case -1 => -1
      case i => unsortedToSorted.image(i)
    }
    new GramMatrix[M, F](generatingMoments, sortedMoments, operatorSymmetries, matrixSymmetries, homomorphism, sortedMomentMatrix, phaseMatrix)
  }
}
*/