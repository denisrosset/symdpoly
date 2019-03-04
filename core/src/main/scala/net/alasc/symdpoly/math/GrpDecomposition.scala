package net.alasc.symdpoly.math

import scala.annotation.tailrec

import spire.algebra.Group

import net.alasc.bsgs.{Chain, GrpChainPermutationAction, Node, Term}
import net.alasc.finite.Grp
import net.alasc.syntax.group._

/** Decomposition of a group as the cartesian product of a sequence of sets, such that every group element can be written
  * as d(1)(i1) |+| d(2)(i2) |+| d(3)(i3) ... for indices i1, i2, i3 ... where d = decomposition
  *
  * Additional property: the first element of each set in the decomposition is the identity.
  */
class GrpDecomposition[G](val transversals: List[Vector[G]])

object GrpDecomposition {

  /** Returns the decomposition of a group. */
  def apply[G](grp: Grp[G])(implicit G: GrpChainPermutationAction[G]): GrpDecomposition[G] = {
    import G.{group, equ}
    val chain = G.fromGrp(grp).chain
    @tailrec def buildTransversal(c: Chain[G, _], acc: List[Vector[G]]): List[Vector[G]] = c match {
      case node: Node[G, _] =>
        val transversal = Group[G].id +: node.orbit.filterNot(_ == node.beta).toVector.map(b => node.u(b))
        buildTransversal(node.next, transversal :: acc)
      case _: Term[G, _] => acc
    }
    new GrpDecomposition[G](buildTransversal(chain, Nil))
  }

  /** Returns the decomposition of the trivial group. */
  def empty[G]: GrpDecomposition[G] = new GrpDecomposition[G](Nil)

}
