package net.alasc.symdpoly
package free

/** Describes a slice of an index of an object with indices, such as a family of operator variables. */
sealed trait Slice {
  def contains(i: Int): Boolean
}

object Slice {

  /** Converts the companion object `::` into a "all elements" slice, to emulate Matlab-like syntax "a(::)" */
  implicit def all(arg: ::.type): Slice = All

  /** Converts a single index into a single element slice. */
  implicit def single(i: Int): Slice = Single(i)

  /** Converts a sequence of integers into a slice. */
  implicit def iterable(iterable: Iterable[Int]): Slice = Explicit(iterable.toSet)

  case object All extends Slice {
    override def toString: String = "::"
    def contains(i: Int): Boolean = true
  }

  case class Single(i: Int) extends Slice {
    override def toString: String = i.toString
    def contains(j: Int): Boolean = i == j
  }

  case class Explicit(set: Set[Int]) extends Slice {
    override def toString: String = set.toSeq.sorted.mkString("{",",","}")
    def contains(i: Int): Boolean = set.contains(i)
  }

}
