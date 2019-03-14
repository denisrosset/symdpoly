package net.alasc.symdpoly.util

import spire.algebra.Order

import scala.collection.immutable.SortedSet

/** Like SortedSet, but using binary search on a sorted array to retrieve elements, the ordering given by Spire's Order typeclass. */
class OrderedSet[A](private[this] val sortedArray: Array[AnyRef]) { lhs =>

  override def equals(o: scala.Any): Boolean = o match {
    case rhs: OrderedSet[_] => lhs.length == rhs.length && {
      (0 until lhs.length).forall(i => lhs(i) == rhs(i))
    }
  }

  override def hashCode: Int = scala.util.hashing.MurmurHash3.arrayHash(sortedArray)

  override def toString: String = sortedArray.map(_.toString).mkString("OrderSet(",", ",")")

  def apply(i: Int): A = sortedArray(i).asInstanceOf[A]
  def length: Int = sortedArray.length

  /** Returns the index of the given elements, or -1 if the element cannot be found. */
  def indexOf(a: A)(implicit ord: Order[A]): Int = {
    /* Linear-time variant for debugging, TODO: remove
    cforRange(0 until length) { i =>
      if (ord.eqv(apply(i), a)) return i
    }
    return -1*/
    val res = spire.math.Searching.search(sortedArray, a.asInstanceOf[AnyRef])(ord.asInstanceOf[Order[AnyRef]])
    if (res < 0) -1 else res
  }

  def contains(a: A)(implicit ord: Order[A]): Boolean = indexOf(a) != -1
  def iterator: Iterator[A] = Iterator.tabulate(length)(apply)

  def toSortedSet(implicit ord: Order[A]): SortedSet[A] = {
    import spire.compat._
    iterator.to[SortedSet]
  }

  def diff(rhs: OrderedSet[A])(implicit ord: Order[A]): OrderedSet[A] =
    OrderedSet.fromSortedSet(lhs.toSortedSet diff rhs.toSortedSet)

  def union(rhs: OrderedSet[A])(implicit ord: Order[A]): OrderedSet[A] =
    OrderedSet.fromSortedSet(lhs.toSortedSet union rhs.toSortedSet)

}

object OrderedSet {
  import spire.compat._

  def empty[A]: OrderedSet[A] = new OrderedSet[A](Array.empty[AnyRef])

  def fromIterator[A:Order](iterator: Iterator[A]): OrderedSet[A] =
    fromSortedSet(iterator.to[SortedSet])

  def apply[A:Order](elements: A*): OrderedSet[A] =
    fromSortedSet(elements.to[SortedSet])

  def fromOrdered[A](seq: Seq[A]): OrderedSet[A] =
    new OrderedSet[A](seq.map(_.asInstanceOf[AnyRef]).toArray)

  def fromUnique[A](iterable: Iterable[A])(implicit ord: Order[A]): OrderedSet[A] = {
    val array = iterable.map(_.asInstanceOf[AnyRef]).toArray
    spire.math.Sorting.quickSort(array)(ord.asInstanceOf[Order[AnyRef]], implicitly)
    new OrderedSet[A](array)
  }

  def fromSortedSet[A](set: SortedSet[A]): OrderedSet[A] =
    new OrderedSet[A](set.iterator.map(_.asInstanceOf[AnyRef]).to[Array])

}
