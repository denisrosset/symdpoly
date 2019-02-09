package net.alasc.symdpoly

import cats.Contravariant
import shapeless.Witness
import spire.algebra.{Group, Involution, Monoid, Order}
import spire.syntax.cfor.cforRange

import scalin.immutable.Mat

import net.alasc.symdpoly.evaluation.{EvaluatedMono2, Evaluator2, SymmetryEquivalence2}
import net.alasc.symdpoly.internal.{MomentSet2, MomentSetBuilder2}
import scalin.immutable.dense._
import spire.syntax.action._

import net.alasc.symdpoly.algebra.Phased.syntax._
import spire.syntax.group._
import spire.syntax.multiplicativeMonoid._
import spire.syntax.involution._
import cats.syntax.invariant._
import cats.syntax.contravariant._
import spire.syntax.action._

import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.GrpChain
import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.symdpoly.algebra.{Morphism, MultiplicativeBinoid, Phased}
import net.alasc.symdpoly.math.{GenPerm, Phases}
import net.alasc.symdpoly.algebra.Instances._
import spire.std.unit._

import net.alasc.perms.default._
import net.alasc.symdpoly.quotient.{QuotientMonoPermutationAction, QuotientPermutation}

trait MatrixSymmetries {
  type G
  def grp: Grp[G]
  def representation: Morphism[G, GenPerm, Group]
  lazy val niceMorphism: Morphism[G, Perm, Group] = {
    val action: PermutationAction[G] = grp match {
      case grpChain: GrpChain[G, a] if grpChain.action.isFaithful => grpChain.action
      case _ =>
        val genPerms = grp.generators.map(representation.apply)
        Contravariant[PermutationAction].contramap(GenPerm.fpab.apply(genPerms))(representation.apply)
    }
    new Morphism[G, Perm, Group] {
      def S: Group[G] = grp.group
      def T: Group[Perm] = Perm.algebra
      def apply(g: G): Perm = action.toPerm(g)
    }
  }
}

object MatrixSymmetries {

  implicit val monoid: Monoid[MatrixSymmetries] = new Monoid[MatrixSymmetries] {
    def empty: MatrixSymmetries = trivial
    def combine(x: MatrixSymmetries, y: MatrixSymmetries): MatrixSymmetries =
      if (x.grp.isTrivial) y
      else if (y.grp.isTrivial) x
      else {
        val g1 = x.grp.generators.map(x.representation)
        val g2 = y.grp.generators.map(y.representation)
        new MatrixSymmetries {
          type G = GenPerm
          def grp: Grp[GenPerm] = Grp(g1 ++ g2: _*)
          def representation: Morphism[GenPerm, GenPerm, Group] = new Morphism[GenPerm, GenPerm, Group] {
            def S: Group[GenPerm] = GenPerm.group
            def T: Group[GenPerm] = GenPerm.group
            def apply(g: GenPerm): GenPerm = g
          }
        }
      }
  }

  val trivial: MatrixSymmetries = new MatrixSymmetries {
    type G = Unit
    def grp: Grp[Unit] = Grp.trivial[Unit]
    def representation: Morphism[Unit, GenPerm, Group] = new Morphism[Unit, GenPerm, Group] {
      def S: Group[Unit] = implicitly
      def T: Group[GenPerm] = GenPerm.group
      def apply(s: Unit): GenPerm = GenPerm.id
    }
  }

  def fromEquivalence[M <: generic.MonoidDef with Singleton:Witness.Aux, G0](e: SymmetryEquivalence2[M, G0], set: OrderedSet[M#Monomial]): MatrixSymmetries =
    new MatrixSymmetries {
      type G = G0
      def grp: Grp[G] = e.grp
      def representation: Morphism[G, GenPerm, Group] = new Morphism[G, GenPerm, Group] {
        def S: Group[G] = grp.group
        def T: Group[GenPerm] = GenPerm.group
        def apply(g: G): GenPerm = {
          import e.action
          implicit def order: Order[M#Monomial] = valueOf[M].monoOrder
          implicit def phased: Phased[M#Monomial] = valueOf[M].monoPhased
          import scala.collection.mutable.{HashMap => MMap}
          val phaseMap: MMap[Int, Phase] = MMap.empty[Int, Phase]
          val n = set.length
          val permImages = new Array[Int](n)
          cforRange(0 until n) { i =>
            val image = set(i) <|+| g
            val canonical = image.phaseCanonical
            val phase = image.phaseOffset
            val permImage = set.indexOf(canonical)
            permImages(i) = permImage
            phaseMap(permImage) = phase
          }
          val perm = Perm.fromImages(permImages)
          val phases = Phases(phaseMap.toVector: _*)
          GenPerm(perm, phases)
        }
      }
    }

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

  lazy val matrixSymmetries: MatrixSymmetries = Monoid[MatrixSymmetries].combineAll(
    E.equivalences.collect {
      case se: SymmetryEquivalence2[M, g] => MatrixSymmetries.fromEquivalence(se, generatingMoments)
    }
  )

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

  def genericConstruction[
    E <: Evaluator2[M] with Singleton,
    M <: generic.MonoidDef with Singleton: Witness.Aux
  ](E: E, gSet: GSet[M]): GramMatrix2[E, M] = {
    implicit def witnessE: Witness.Aux[E] = (E: E).witness
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
          val phased: EvaluatedMono2[E, M] = (E: E)(generatingMoments(r).adjoint * generatingMoments(c))
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
            val phasedAdj = (E: E)(generatingMoments(c).adjoint * generatingMoments(r))
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

  def freeBasedConstruction[
    E <: Evaluator2[M] with Singleton,
    M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton: Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton](E: E, gSet: GSet[M]): GramMatrix2[E, M] = {
   /* val evaluators = E.equivalences.map {
      case e: SymmetryEquivalence2 =>
        e.action match {
          case qmpa: QuotientMonoPermutationAction[M, F] =>
            ???
        }
    }*/
    ???
  }

  def apply[
    E <: Evaluator2[M] with Singleton,
    M <: generic.MonoidDef with Singleton: Witness.Aux
  ](E: E, gSet: GSet[M]): GramMatrix2[E, M] = valueOf[M] match {
    case m: generic.FreeBasedMonoidDef =>
      val res = freeBasedConstruction[
        Evaluator2[m.type] with Singleton,
        m.type with generic.FreeBasedMonoidDef.Aux[m.Free] with Singleton, m.Free
        ](E.asInstanceOf[Evaluator2[m.type] with Singleton], gSet.asInstanceOf[GSet[m.type]])(m.witness.asInstanceOf[Witness.Aux[m.type with generic.FreeBasedMonoidDef.Aux[m.Free] with Singleton]])
      res.asInstanceOf[GramMatrix2[E, M]]
    case _ => genericConstruction[E, M](E, gSet)
  }

}