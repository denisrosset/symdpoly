package net.alasc.symdpoly
package free

sealed trait Slice {
  def contains(i: Int): Boolean
}

object Slice {
  implicit def all(arg: ::.type): Slice = All
  implicit def single(i: Int): Slice = Single(i)
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
