package net.alasc.symdpoly

import shapeless.Witness
import spire.algebra.Action
import spire.syntax.action._
import spire.syntax.cfor._

import scalin.immutable.Mat
import scalin.immutable.dense._

import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.perms.default._
import net.alasc.symdpoly
import net.alasc.symdpoly.evaluation.FreeBasedEvaluator
import net.alasc.symdpoly.internal.{MomentSet, MomentSetBuilder}
import net.alasc.symdpoly.math.{GenPerm, PhasedInt, Phases}
import net.alasc.util._

class GramMatrix[
  M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
](val generatingMoments: OrderedSet[Mono[M, F]],
  val momentSet: MomentSet[M, F],
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
    F <: free.MonoidDef.Aux[F] with Singleton
  ](gSet: GSet[M], evaluator: FreeBasedEvaluator[M, F], symmetryGroup: Grp[GenPerm] = Grp.trivial[GenPerm])(implicit wM: Witness.Aux[M]): GramMatrix[M, F] = {

    def M: M = wM.value
    implicit def wF: Witness.Aux[F] = (M.Free: F).witness
    val generatingMoments = OrderedSet.fromOrdered(gSet.monomials.toVector)
    val n = generatingMoments.length
    val generatingMomentsAdjoint = Array.tabulate(n)(i => generatingMoments(i).adjoint)
    val maxDegree = Iterator.range(0, generatingMoments.length).map(i => generatingMoments(i).data.length).max
    val sb = MomentSetBuilder.make[F]

    val monoPerms = Grp(symmetryGroup.generators.map(momentSetAction(generatingMoments, _)): _*).iterator.toVector
    val groupElements = symmetryGroup.iterator.toArray
    
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
    new GramMatrix[M, F](generatingMoments, sortedMoments, sortedMomentMatrix, phaseMatrix)
  }
}
