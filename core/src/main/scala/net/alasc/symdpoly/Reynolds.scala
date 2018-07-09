package net.alasc.symdpoly

import net.alasc.finite.Grp
import spire.algebra.{Action, VectorSpace}

/** Reynolds operator that averages the orbit of an element under the action of a group. */
object Reynolds {

  def apply[G, P, R](p: P, grp: Grp[G])(implicit P: VectorSpace[P, R], G: Action[P, G]): P = {
    val sum = grp.iterator.map(g => G.actr(p, g)).reduce(P.plus)
    P.divr(sum, P.scalar.fromBigInt(grp.order.toBigInt))
  }

}
