package net.alasc.symdpoly.sdp

import spire.algebra.Group

import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.symdpoly.algebra.Morphism
import net.alasc.symdpoly.math.GenPerm

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

}
