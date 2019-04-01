package net.alasc.symdpoly
package algebra

import scala.collection.immutable.{SortedMap, SortedSet}

import spire.algebra.{Eq, Order, VectorSpace}

/** A nice vector space:
  *
  * - has an ordered basis,
  *
  * @tparam V Type of vector space elements
  * @tparam B Type of basis elements
  * @tparam F Scalar
  */
trait NiceVectorSpace[V, B, F] extends VectorSpace[V, F] { self =>
  implicit def eqF: Eq[F]
  implicit def orderB: Order[B]
  /** Returns the support of a vector element as the set of basis elements over which its linear decomposition has nonzero coefficients. */
  def support(v: V): SortedSet[B] = decompose(v).keySet
  def decompose(v: V): SortedMap[B, F]
  def basisElement(b: B): V
  def element(f: F, b: B): V = timesl(f, basisElement(b))
  def linearCombination(elements: Iterable[(B, F)]): V = sum(elements.map { case (b, f) => element(f, b) })
}

object NiceVectorSpace {

  def apply[V, B, F](implicit ev: NiceVectorSpace[V, B, F]): NiceVectorSpace[V, B, F] = ev

}
