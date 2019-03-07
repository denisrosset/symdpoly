package net.alasc.symdpoly
package algebra

import net.alasc.algebra.PermutationAction
import net.alasc.attributes.Attributable
import net.alasc.finite._
import spire.syntax.group._
import spire.algebra.{Eq, Group}

import net.alasc.perms.default._
import net.alasc.std.product._
import spire.std.tuples._
import instances.invariant._
import net.alasc.syntax.all._
import net.alasc.util.NNOption
import shapeless.Witness
import scala.annotation.tailrec
import scala.reflect.ClassTag

import cats.{Contravariant, Invariant, InvariantMonoidal}
import spire.math.SafeLong

import net.alasc.perms.Perm

/** Describes a map S => T that preserves the algebraic structure F[_] */
trait Morphism[S, T, F[_]] extends Function1[S, T] {
  def S: F[S]
  def T: F[T]
  def apply(s: S): T
}

object Morphism {

  /** Constructs a morphism from a S => T function. */
  def apply[S, T, F[_]](f: S => T)(implicit S0: F[S], T0: F[T]): Morphism[S, T, F] = new Morphism[S, T, F] {
    def S: F[S] = S0
    def T: F[T] = T0
    def apply(s: S): T = f(s)
  }

  /** Constructs a morphism for a net.alasc.finite.Grp using images of the generators. */
  def fromGeneratorImages[S, T, F[_]](source: Grp[S], images: Seq[T])(implicit ev: MorphismFromGeneratorImages[S, T]): Morphism[S, T, Group] =
    ev(source, images)

  /** Enrichment methods for group morphisms. */
  implicit class morphismGrpImage[S, T](val morphism: Morphism[S, T, Group]) extends AnyVal {

    /** Returns the image of a net.alasc.finite.Grp under a group morphism. */
    def grpImage(source: Grp[S])(implicit equ: Eq[T], group: Group[T], grpGroup: GrpGroup[T]): Grp[T] = {
      val imageGenerators = source.generators.map(morphism).filterNot(_.isEmpty)
      grpGroup.fromGenerators(imageGenerators)
    }

  }

}

/** Describes a morphism S => T that is also surjective, i.e. every element of T has a preimage, not necessarily unique.*/
trait SurjectiveMorphism[S, T, F[_]] extends Morphism[S, T, F] {
  def preimageRepresentative(t: T): S
}

object SurjectiveMorphism {

  /** Constructs a surjective morphism from a pair of functions S => T and T => S,
    * such that their composition T => S => T is the identity. */
  def apply[S, T, F[_]](image: S => T)(preimage: T => S)(implicit S0: F[S], T0: F[T]): SurjectiveMorphism[S, T, F] = new SurjectiveMorphism[S, T, F] {
    def S: F[S] = S0
    def T: F[T] = T0
    def apply(s: S): T = image(s)
    def preimageRepresentative(t: T): S = preimage(t)
  }

}

/** Describes the ability to construct a morphism from images of the generators. */
trait MorphismFromGeneratorImages[S, T] {

  def apply(source: Grp[S], images: Seq[T]): Morphism[S, T, Group]
  
}

object MorphismFromGeneratorImages {

  /** Ability to construct morphisms from generator images, if the source group has a faithful permutation action. */
  implicit def forFaithfulPermutationAction[S:Eq:FaithfulPermutationActionBuilder:Group, T:Eq:FaithfulPermutationActionBuilder:Group]: MorphismFromGeneratorImages[S, T] =
    new MorphismFromGeneratorImages[S, T] {
    def apply(source: Grp[S], images: Seq[T]): Morphism[S, T, Group] = {
      import net.alasc.perms.default._ // TODO: parameterize
      val sourceGenerators = source.generators
      val targetGenerators = images.filterNot(_.isEmpty)
      val sAction: PermutationAction[S] = FaithfulPermutationActionBuilder[S].apply(sourceGenerators)
      val combinedAction: PermutationAction[(S, T)] = Contravariant[PermutationAction].contramap[S, (S, T)](sAction)(_._1)
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
