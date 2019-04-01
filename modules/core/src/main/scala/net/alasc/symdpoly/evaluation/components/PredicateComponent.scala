package net.alasc.symdpoly
package evaluation
package components

import scala.collection.immutable.BitSet

import cats.Contravariant
import shapeless.Witness

import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.symdpoly.math.Phase
import net.alasc.symdpoly.util.OrderedSet
import net.alasc.perms.default._
import instances.invariant._
import net.alasc.symdpoly.freebased.{Mono, Permutation}

abstract class PredicateComponent[
  M <: freebased.MonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
] extends InPlaceComponent[M, F]  {

  def predicate: OpPredicate[F]

  val predicateIndex: BitSet = BitSet.empty ++ (0 until F.nOperators).filter(i => predicate(F.opFromIndex(i)))

}
