package net.alasc.symdpoly

import cats.Contravariant

import net.alasc.symdpoly.evaluation.{EvaluatedMono, Evaluator, FreeBasedEvaluator, SymmetryEquivalence}
import net.alasc.symdpoly.internal.{MomentSet, MomentSetBuilder}
import scalin.immutable.dense._
import spire.syntax.action._

import net.alasc.symdpoly.algebra.Phased.syntax._
import spire.syntax.group._
import spire.syntax.multiplicativeMonoid._
import spire.syntax.involution._
import cats.syntax.invariant._
import cats.syntax.contravariant._
import shapeless.Witness
import spire.algebra.{Group, Monoid, Order}
import spire.syntax.action._
import spire.syntax.cfor._

import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.GrpChain
import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.symdpoly.algebra.{Morphism, MultiplicativeBinoid, Phased}
import net.alasc.symdpoly.math.{GenPerm, Phases}
import spire.std.unit._

import net.alasc.perms.default._
import net.alasc.symdpoly.generic.FreeBasedMono
import net.alasc.util.Tuple2Int


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

  def fromEquivalence[M <: generic.MonoidDef with Singleton:Witness.Aux, G0](e: SymmetryEquivalence[M, G0], set: OrderedSet[M#Monomial]): MatrixSymmetries =
    new MatrixSymmetries {
      type G = G0
      def grp: Grp[G] = e.grp
      def representation: Morphism[G, GenPerm, Group] = new Morphism[G, GenPerm, Group] {
        def S: Group[G] = grp.group
        def T: Group[GenPerm] = GenPerm.group
        def apply(g: G): GenPerm = {
          implicit def order: Order[M#Monomial] = valueOf[M].monoOrder
          implicit def phased: Phased[M#Monomial] = valueOf[M].monoPhased
          import e.action
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