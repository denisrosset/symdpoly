package net.alasc.symdpoly
package evaluation

import cats.Contravariant
import shapeless.Witness
import net.alasc.finite.Grp
import instances.all._
import net.alasc.algebra.PermutationAction
import net.alasc.perms.Perm

/** An equivalence relation on monomials. */
trait Equivalence[M <: generic.MonoidDef with Singleton] extends Component[M] { self =>

  /** Returns whether the given group is compatible with this equivalence relation. */
  def isCompatibleGroup(grp: Grp[M#Permutation]): Boolean = grp === compatibleSubgroup(grp)

  /** Returns the group of permutations that is compatible with the evaluator,
    *
    * A group of permutations is deemed compatible if, for any monomials m1 and m2 of type M#Monomial,
    * we have m1 ~ m2 if and only if (m1 <|+| g) ~ (m2 <|+| g) for any element g of the group,
    * where ~ is described by this equivalence relation.
    */
  def compatibleSubgroup(grp: Grp[M#Permutation]): Grp[M#Permutation]

}
