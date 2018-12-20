package net.alasc.symdpoly.algebra

import net.alasc.algebra.PermutationAction
import net.alasc.attributes.Attributable
import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp, GrpGroup}
import spire.algebra.{Eq, Group}
import net.alasc.perms.default._
import net.alasc.std.product._
import spire.std.tuples._
import net.alasc.syntax.all._
import net.alasc.util.NNOption

import scala.reflect.ClassTag

trait Morphism[S, T, F[_]] extends Function1[S, T] with Attributable {
  def S: F[S]
  def T: F[T]
  def apply(s: S): T
}

trait Homomorphism[S, T] extends Morphism[S, T, Group] {
  def S: Group[S]
  def T: Group[T]
  def source: Grp[S]
  def target: Grp[T]
}

/** Describes an homomorphism between group. TODO: make more efficient! */
class HomomorphismByImages[
  S:ClassTag:Eq:FaithfulPermutationActionBuilder,
  T:ClassTag:Eq:FaithfulPermutationActionBuilder
](val source: Grp[S], val images: Seq[T])(implicit val S: Group[S], val T: Group[T]) extends Homomorphism[S, T] {
  require(source.nGenerators == images.length)
  val target: Grp[T] = Grp(images: _*)
  private def generatorsS = source.generators
  private def generatorsT = images
  private val sAction: PermutationAction[S] = FaithfulPermutationActionBuilder[S].apply(source)
  private val combinedAction: PermutationAction[(S, T)] = new PermutationAction[(S, T)] {
    def isFaithful: Boolean = true
    def findMovedPoint(g: (S, T)): NNOption = sAction.findMovedPoint(g._1)
    def movedPointsUpperBound(g: (S, T)): NNOption = sAction.movedPointsUpperBound(g._1)
    def actl(g: (S, T), p: Int): Int = sAction.actl(g._1, p)
    def actr(p: Int, g: (S, T)): Int = sAction.actr(p, g._1)
  }
  private val combinedGrp: Grp[(S, T)] = Grp(generatorsS zip generatorsT: _*)
  val nPoints = combinedGrp.largestMovedPoint(combinedAction).getOrElse(-1) + 1
  assert(combinedGrp.pointwiseStabilizer(combinedAction, 0 until nPoints: _*).isTrivial,
    "The given images do not prescribe an homomorphism")
  def apply(s: S): T = combinedGrp.findSameAction(combinedAction, s)(sAction, implicitly).get._2
}

object Homomorphism {
  def fromGeneratorImages[
    S:ClassTag:Eq:FaithfulPermutationActionBuilder:Group,
    T:ClassTag:Eq:FaithfulPermutationActionBuilder:Group
  ](source: Grp[S], images: Seq[T]): Homomorphism[S, T] = new HomomorphismByImages[S, T](source, images)

  // TODO: do not construct the BSGS morphism, and add applyUnsafe method
  def apply[
    S:ClassTag:Eq:FaithfulPermutationActionBuilder:Group,
    T:ClassTag:Eq:FaithfulPermutationActionBuilder:Group
  ](source: Grp[S], hom: S => T): Homomorphism[S, T] = new HomomorphismByImages[S, T](source, source.generators.map(hom))

}
