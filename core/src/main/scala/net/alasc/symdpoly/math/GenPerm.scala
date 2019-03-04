package net.alasc.symdpoly.math

import net.alasc.algebra.PermutationAction
import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp, GrpGroup}
import net.alasc.named.{Cyclic, Symmetric}
import net.alasc.perms.Perm
import net.alasc.util._
import spire.algebra._
import spire.math.SafeLong
import spire.syntax.eq._
import scala.annotation.tailrec

import net.alasc.symdpoly.Phase

/** A generalized permutation is an element of the generalized symmetric group, and combines
  * a permutation and a multiplication by a diagonal matrix whose entries are rational roots
  * of unity.
  *
  * It naturally acts on integers associated with a phase: [[PhasedInt]].
  */
case class GenPerm(val perm: Perm, val phases: Phases) { lhs =>

  override def equals(any: Any): Boolean = any match {
    case rhs: GenPerm => (lhs.perm === rhs.perm) && (lhs.phases === rhs.phases)
    case _ => false
  }

  override def hashCode: Int = perm.hashCode + 41 * phases.hash

  def image(pi: PhasedInt): PhasedInt = {
    val newIndex = perm.image(pi.index)
    val newPhase = pi.phase * phases.phaseFor(newIndex)
    PhasedInt(newPhase, newIndex)
  }

  def invImage(pi: PhasedInt): PhasedInt = {
    val newIndex = perm.invImage(pi.index)
    val newPhase = pi.phase * phases.phaseFor(pi.index).reciprocal
    PhasedInt(newPhase, newIndex)
  }

  def largestMovedPoint: NNOption = {
    val lmpPhase = if (phases.size == 0) -1 else phases.key(phases.size - 1)
    val lmpPerm = perm.largestMovedPoint.getOrElseFast(-1)
    NNOption(spire.math.max(lmpPhase, lmpPerm))
  }

  def |+|(rhs: GenPerm): GenPerm = {
    val newPerm = lhs.perm |+| rhs.perm
    val newPhases = lhs.phases.mapKeys(rhs.perm) |+| rhs.phases
    new GenPerm(newPerm, newPhases)
  }

  def inverse: GenPerm = {
    val invPerm = perm.inverse
    val invPhases = phases.mapKeys(invPerm).inverse
    new GenPerm(invPerm, invPhases)
  }

  /** Assuming that 0...n-1 is an orbit of this GenPerm, returns the GenPerm that
    * acts only on that orbit.
    */
  def truncate(n: Int): GenPerm = {
    val permImages = Array.tabulate(n) { i =>
      val image = perm.image(i)
      assert(image < n)
      image
    }
    GenPerm(Perm.fromImages(permImages), phases.truncate(n))
  }

}

protected[math] final class GenPermInstances extends Eq[GenPerm] with Group[GenPerm] {
  def eqv(x: GenPerm, y: GenPerm): Boolean = x == y
  def inverse(a: GenPerm): GenPerm = a.inverse
  def empty: GenPerm = GenPerm.id
  def combine(x: GenPerm, y: GenPerm): GenPerm = x |+| y
}

protected[math] final class PhasedIntGenPermAction extends Action[PhasedInt, GenPerm] {
  def actr(p: PhasedInt, g: GenPerm): PhasedInt = g.image(p)
  def actl(g: GenPerm, p: PhasedInt): PhasedInt = g.invImage(p)
}

protected[math] final case class GenPermFaithfulPermutationAction(domainSize: Int, rootOrder: Int) extends PermutationAction[GenPerm] {
  def isFaithful: Boolean = true
  def findMovedPoint(g: GenPerm): NNOption = g.perm.smallestMovedPoint match {
    case NNOption(i) => NNSome(i * rootOrder)
    case _ if g.phases.size > 0 => NNOption(g.phases.key(0) * rootOrder)
    case _ => NNNone
  }
  def movedPointsUpperBound(g: GenPerm): NNOption = NNOption(domainSize*rootOrder - 1)
  def actr(p: Int, g: GenPerm): Int = {
    val phase = Phase(p % rootOrder, rootOrder)
    val index = p / rootOrder
    val PhasedInt(newPhase, newIndex) = g.image(PhasedInt(phase, index))
    newIndex * rootOrder + newPhase.numeratorIn(rootOrder)
  }
  def actl(g: GenPerm, p: Int): Int = {
    val phase = Phase(p % rootOrder, rootOrder)
    val index = p / rootOrder
    val PhasedInt(newPhase, newIndex) = g.invImage(PhasedInt(phase, index))
    newIndex * rootOrder + newPhase.numeratorIn(rootOrder)
  }
}


protected[math] final class GenPermFaithfulPermutationActionBuilder extends FaithfulPermutationActionBuilder[GenPerm] {
  def apply(generators: Iterable[GenPerm]): PermutationAction[GenPerm] = {
    // computes the max of the largest moved points and the lcm of the root order
    @tailrec def iter(it: Iterator[GenPerm], domainSize: Int, rootOrder: Int): (Int, Int) =
      if (!it.hasNext) (domainSize, rootOrder) else {
        val gp = it.next()
        val gpRootOrder = gp.phases.commonRootOrder
        val gpDomainSize = gp.largestMovedPoint.getOrElseFast(-1)
        iter(it, spire.math.max(domainSize, gpDomainSize), spire.math.lcm(rootOrder.toLong, gpRootOrder.toLong).toInt)
      }
    val (fpaDomainSize, fpaRootOrder) = iter(generators.iterator, -1, 1)
    new GenPermFaithfulPermutationAction(fpaDomainSize + 1, fpaRootOrder)
  }
}

object GenPerm {

  /** Identity element. */
  val id: GenPerm = GenPerm(Perm.id, Phases.empty)

  /** Private instance for [[Group]] and [[Eq]] of [[GenPerm]]. */
  private[this] val instances: GenPermInstances = new GenPermInstances

  /** Builder for faithful permutation representations. */
  implicit val fpab: FaithfulPermutationActionBuilder[GenPerm] = new GenPermFaithfulPermutationActionBuilder

  /** Group instance for generalized permutations. */
  implicit def group: Group[GenPerm] = instances

  /** Eq instance for generalized permutations. */
  implicit def equ: Eq[GenPerm] = instances

  /** Action of generalized permutations on integers with a phase. */
  implicit val phasedIntAction: Action[PhasedInt, GenPerm] = new PhasedIntGenPermAction

  /** Returns the generalized symmetric group, i.e. the wreath product of the cyclic group
    * of order m and the symmetric group of order n.
    */
  def generalizedSymmetricGroup(m: Int, n: Int)(implicit gg: GrpGroup[GenPerm]): Grp[GenPerm] = {
    val sGens = if (n > 1) Vector(Symmetric.transposition(0, 1), Cyclic.shift(n)).map(GenPerm(_, Phases.empty)) else Vector.empty
    val sOrder = Symmetric.order(n)
    if (m == 1) Grp.fromGeneratorsAndOrder(sGens, sOrder) else {
      val pGens = Vector.tabulate(n)(i => GenPerm(Perm.id, Phases((i, Phase.apply(1, m)))))
      val pOrder = SafeLong(m).pow(n)
      Grp.fromGeneratorsAndOrder(pGens ++ sGens, pOrder * sOrder)
    }
  }

  /** Finds the least common multiple of the phase denominators present in a sequence of generalized permutations. */
  def commonRootOrder(perms: Iterable[GenPerm]): Int = {
    // computes the max of the largest moved points and the lcm of the root order
    @tailrec def iter(it: Iterator[GenPerm], rootOrder: Int): Int =
      if (!it.hasNext) rootOrder else {
        val gp = it.next()
        iter(it, spire.math.lcm(rootOrder.toLong, gp.phases.commonRootOrder.toLong).toInt)
      }
    iter(perms.iterator, 1)
  }

}
