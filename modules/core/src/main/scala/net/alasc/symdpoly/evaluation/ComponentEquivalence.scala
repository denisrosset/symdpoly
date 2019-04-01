package net.alasc.symdpoly
package evaluation

import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.symdpoly.math.{GenPerm, Phases}
import shapeless.Witness
import net.alasc.perms.default._

/** An equivalence relation composed of components.
  *
  * See documentation of parameters in [[Equivalence.apply]]. */
final class ComponentEquivalence[
  M <: freebased.MonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
](components: Seq[Component[M]],
  permutationCompatible: M#PermutationType => Boolean,
  actionCompatible: (F#Op, F#Op) => Boolean)
 (implicit val witnessM: Witness.Aux[M]) extends Equivalence[M] with FreeBasedComponent[M, F] {

  private[this] val action: PermutationAction[M#PermutationType] =
    instances.invariant.symdpolyContravariantForPermutationAction
      .contramap[Perm, M#PermutationType](Perm.algebra)(_.genPerm.perm)

  private[this] def predicate(p: Perm): Boolean =
    permutationCompatible(new freebased.Permutation[M, F](GenPerm(p, Phases.empty)))

  private[this] def backtrackTest(i: Int, j: Int): Boolean =
    actionCompatible(F.opIndexMap.elements(i), F.opIndexMap.elements(j))

  def apply(mono: M#MonoType): Set[M#MonoType] =
    components.foldLeft(Set(mono)) {
      case (res, equiv) => res.flatMap(m => equiv.apply(m))
    }

  def compatibleSubgroup(grp: Grp[M#PermutationType]): Grp[M#PermutationType] = {
    grp.subgroupFor(action, backtrackTest, predicate)
  }

  def isSelfAdjoint: Boolean = components.lastOption.fold(false)(_.isSelfAdjoint)

}
