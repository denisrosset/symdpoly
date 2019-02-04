package net.alasc.symdpoly.algebra

import net.alasc.algebra.PermutationAction
import net.alasc.attributes.Attributable
import net.alasc.finite._
import spire.syntax.group._
import spire.algebra.{Eq, Group}
import net.alasc.perms.default._
import net.alasc.std.product._
import net.alasc.symdpoly.free.{FreeGroup, Word}
import spire.std.tuples._
import net.alasc.syntax.all._
import net.alasc.util.NNOption
import shapeless.Witness

import scala.annotation.tailrec
import scala.reflect.ClassTag

trait Morphism[S, T, F[_]] extends Function1[S, T] with Attributable {
  def S: F[S]
  def T: F[T]
  def apply(s: S): T
}

object Morphism {

  def apply[S, T, F[_]](f: S => T)(implicit S0: F[S], T0: F[T]): Morphism[S, T, F] = new Morphism[S, T, F] {
    def S: F[S] = S0
    def T: F[T] = T0
    def apply(s: S): T = f(s)
  }

  def fromGeneratorImages[S, T, F[_]](source: FinitelyGeneratedGrp.Aux[S], images: Seq[T])(implicit ev: MorphismFromGeneratorImages[S, T]): Morphism[S, T, Group] =
    ev(source, images)

  implicit class morphismGrpImage[S, T](val morphism: Morphism[S, T, Group]) extends AnyVal {
    def grpImage(source: Grp[S])(implicit equ: Eq[T], group: Group[T], grpGroup: GrpGroup[T]): Grp[T] = {
      val imageGenerators = source.generators.map(morphism).filterNot(_.isEmpty)
      grpGroup.fromGenerators(imageGenerators)
    }
  }
}


trait MorphismFromGeneratorImages[S, T] {

  def apply(source: FinitelyGeneratedGrp.Aux[S], images: Seq[T]): Morphism[S, T, Group]
}

abstract class MorphismFromGeneratorImages0 {

  implicit def forFreeGroupPermutationAction[Source <: FreeGroup with Singleton, T:Eq:Group](implicit sourceW: Witness.Aux[Source]): MorphismFromGeneratorImages[Word[Source], T] =
    new MorphismFromGeneratorImages[Word[Source], T] {
      def apply(source: FinitelyGeneratedGrp.Aux[Word[Source]], images: Seq[T]): Morphism[Word[Source], T, Group] =
        new Morphism[Word[Source], T, Group] {
          def S: Group[Word[Source]] = (sourceW.value: Source).group
          def T: Group[T] = implicitly
          def apply(word: Word[Source]): T = {
            @tailrec def iter(a: T, i: Int): T =
              if (i == word.length) a
              else if (word.sign(i) == 1) iter(a |+| images(word.index(i)), i + 1)
              else iter(a |-| images(word.index(i)), i + 1)
            iter(Group[T].empty, 0)
          }
        }
    }
}


object MorphismFromGeneratorImages extends MorphismFromGeneratorImages0 {

  implicit def forFaithfulPermutationAction[S:Eq:FaithfulPermutationActionBuilder:Group, T:Eq:FaithfulPermutationActionBuilder:Group]: MorphismFromGeneratorImages[S, T] =
    new MorphismFromGeneratorImages[S, T] {
    def apply(source: FinitelyGeneratedGrp.Aux[S], images: Seq[T]): Morphism[S, T, Group] = {
      import net.alasc.perms.default._ // TODO: parameterize
      val sourceGenerators = source.generators
      val targetGenerators = images.filterNot(_.isEmpty)
      val sAction: PermutationAction[S] = FaithfulPermutationActionBuilder[S].apply(sourceGenerators)
      val combinedAction: PermutationAction[(S, T)] = new PermutationAction[(S, T)] {
        def isFaithful: Boolean = true
        def findMovedPoint(g: (S, T)): NNOption = sAction.findMovedPoint(g._1)
        def movedPointsUpperBound(g: (S, T)): NNOption = sAction.movedPointsUpperBound(g._1)
        def actl(g: (S, T), p: Int): Int = sAction.actl(g._1, p)
        def actr(p: Int, g: (S, T)): Int = sAction.actr(p, g._1)
      }
      val combinedGrp: Grp[(S, T)] = Grp(sourceGenerators zip targetGenerators: _*)
      val nPoints = combinedGrp.largestMovedPoint(combinedAction).getOrElse(-1) + 1
      assert(combinedGrp.pointwiseStabilizer(combinedAction, 0 until nPoints: _*).isTrivial,
        "The given images do not prescribe an homomorphism")
      new Morphism[S, T, Group] {
        def S: Group[S] = implicitly
        def T: Group[T] = implicitly
        def apply(s: S): T =  combinedGrp.findSameAction(combinedAction, s)(sAction, implicitly).get._2
      }
    }
  }

}
