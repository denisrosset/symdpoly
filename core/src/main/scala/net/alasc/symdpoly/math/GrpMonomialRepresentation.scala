package net.alasc.symdpoly
package math

import scala.reflect.ClassTag

import cats.Contravariant
import spire.algebra.{Action, Eq, Group, Order}

import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.GrpChain
import net.alasc.finite.{Grp, GrpGroup, GrpPermutationAction}
import net.alasc.perms.Perm
import net.alasc.symdpoly.algebra.{InjectiveMorphism, Morphism, MorphismFromGeneratorImages, Phased}
import net.alasc.perms.default._
import net.alasc.symdpoly.util.OrderedSet
import syntax.phased._
import instances.invariant._
import net.alasc.perms.default._

/** Describes a finite group and one of its monomial unitary representations.
  *
  * @param grp Finite group being represented.
  * @param n   Dimension of the representation.
  * @param representation Representation written as a group homomorphism to generalized permutations.
  */
case class GrpMonomialRepresentation[G](grp: Grp[G], n: Int, representation: Morphism[G, GenPerm, Group]) {

  def generatorImages: Seq[GenPerm] = grp.generators.map(representation.apply)

  /** Nice injective morphism from G to permutations, used to study the group. */
  lazy val niceMorphism: InjectiveMorphism[G, Perm, Group] = {
    val action: PermutationAction[G] = grp match {
      case grpChain: GrpChain[G, a] if grpChain.action.isFaithful => grpChain.action
      case _ => instances.invariant.ContravariantForFaithfulPermutationAction.contramap(GenPerm.fpab.apply(generatorImages))(representation.apply)
    }
    InjectiveMorphism[G, Perm, Group](action.toPerm)(representation.S, implicitly)
  }

  /** Returns a conversion of this representation such that the underlying group is a permutation group. */
  lazy val onPermutationGroup: GrpMonomialRepresentation[Perm] = {
    val permGenerators = grp.generators.map(niceMorphism)
    val grp1 = Grp.fromGeneratorsAndOrder(permGenerators, grp.order)
    val representation1 = Morphism.fromGeneratorImages(grp1, generatorImages)
    GrpMonomialRepresentation(grp1, n, representation1)
  }

}

object GrpMonomialRepresentation {

  /** Constructs a representation of the trivial group of the given dimension. */
  def trivial[G:Eq:Group](n: Int): GrpMonomialRepresentation[G] = GrpMonomialRepresentation(Grp.trivial[G], n, Morphism(x => GenPerm.id))

  /** Constructs a group and its monomial representation from the action of a subgroup compatible with an action.
    *
    * The return group is a subgroup of the given group if the action is not compatible with the given set.
    */
  def fromActionOnOrderedSet[A:ClassTag:Order:Phased, G:GrpPermutationAction](set: OrderedSet[A], grp: Grp[G])
                                                                             (implicit action: Action[A, G], mfgi: MorphismFromGeneratorImages[G, GenPerm]): GrpMonomialRepresentation[G] = {
    val restrictedGrp = symmetries.Orbit.compatibleSubgroup[A, G](set.toIndexedSeq, grp, _.phaseCanonical)
    val generatorImages = restrictedGrp.generators.map(GenPerm.fromActionOnOrderedSet(set, _))
    val representation = Morphism.fromGeneratorImages(restrictedGrp, generatorImages)
    GrpMonomialRepresentation(restrictedGrp, set.length, representation)
  }

}
