package net.alasc.symdpoly.sdp

import spire.algebra.Group

import net.alasc.finite.Grp
import net.alasc.perms.Perm
import cats.syntax.compose._
import net.alasc.perms.default._
import net.alasc.symdpoly.algebra.Morphism
import net.alasc.symdpoly.math.GenPerm
import net.alasc.symdpoly.symmetries.SymmetricGroup
import net.alasc.symdpoly.symmetries.SymmetricGroup.Presentation

case class SDPConstraint(basisSize: Int, blocks: Seq[Block], symmetryGroup: Grp[Perm], representation: Morphism[Perm, RepMat, Group]) {

  def adding(blocks1: Seq[Block], representation1: Morphism[Perm, RepMat, Group]): SDPConstraint = {
    val newBlocks = blocks ++ blocks1
    val newSize = newBlocks.map(_.size).reduce(_ + _)
    val newId = GenPermMat.RealMat(blocks1.map(_.size).reduce(_ + _), GenPerm.id)
    val newGroup = RepMat.groupInstance(RepMat(representation(Perm.id).blocks :+ newId))
    val newRepresentation = Morphism[Perm, RepMat, Group] { perm =>
      RepMat(representation(perm).blocks ++ representation1(perm).blocks)
    }(implicitly, newGroup)
    SDPConstraint(basisSize, newBlocks, symmetryGroup, newRepresentation)
  }

  def mergeBlocks: SDPConstraint =
    SDPConstraint(basisSize, Seq(Block.directSum(basisSize).combineAll(blocks)), symmetryGroup, representation)

  protected def replaceSymmetryGroup(newGroup: Grp[Perm], morphism: Morphism[Perm, Perm, Group]): SDPConstraint =
    SDPConstraint(basisSize, blocks, newGroup, Morphism.compose[Group].andThen(morphism, representation))

  def recognizeSymmetricGroup: SDPConstraint =
    if (symmetryGroup.isTrivial) this
    else {
      if (symmetryGroup.order == 2) {
        val g = symmetryGroup.generator(0)
        val newGroup = Grp.fromGenerators(Vector(Perm(0, 1)))
        val morphism = Morphism.fromGeneratorImages[Perm, Perm, Group](newGroup, Vector(g))
        replaceSymmetryGroup(newGroup, morphism)
      } else SymmetricGroup.tryRecognize(symmetryGroup).fold(this) {
        case pres@Presentation(n, s, t) =>
          val newGroup = Grp.fromGeneratorsAndOrder(Vector(pres.sCanonical, pres.tCanonical), symmetryGroup.order)
          val morphism = Morphism.fromGeneratorImages[Perm, Perm, Group](newGroup, Vector(s, t))
          replaceSymmetryGroup(newGroup, morphism)
      }
    }

}
