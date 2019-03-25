package net.alasc.symdpoly
package evaluation

import cats.Contravariant
import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.symdpoly.math.{GenPerm, Phases}
import shapeless.Witness
import net.alasc.perms.default._
import components._

/** A component of an equivalence relation on monomials. */
trait Component[M <: generic.MonoidDef with Singleton] { self =>

  implicit def witnessM: Witness.Aux[M]
  def M: M = valueOf[M]

  /** Returns the set of monomials equivalent to the given monomial. */
  def apply(mono: M#Monomial): Set[M#Monomial]

  /** Returns whether m.adjoint is equivalent to m for all m of type M#Monomial. */
  def isSelfAdjoint: Boolean

}

object Component {

  /** Equivalence under cyclic permutation of operators selected by the given predicate. */
  def cyclic[
    M <: freebased.MonoidDef.Aux[F] with Singleton : Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](predicate: OpPredicate[F]): FreeBasedComponent[M, F] = CyclicComponent[M, F](predicate)

  /** Equivalence under transposition of the operators selected by the given predicate. */
  def transpose[
    M <: freebased.MonoidDef.Aux[F] with Singleton : Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](predicate: OpPredicate[F]): FreeBasedComponent[M, F] = TransposeComponent[M, F](predicate)

}

