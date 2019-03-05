package net.alasc.symdpoly.util

/** A bidirectional map of elements to indices and vice versa.
  *
  * The parameters given at construction time must respect that indexMap(elements(i)) == i.
  *
  * The elements of type [[A]] must implement the [[java.lang.Object]] hashCode and equals methods.
  */
class IndexMap[A](val indexMap: Map[A, Int], val elements: Seq[A]) {
  def size: Int = elements.size
}

object IndexMap {

  /** Constructs an [[IndexMap]] from the given sequence of elements. */
  def apply[A](elements: Seq[A]): IndexMap[A] = {
    val indices = elements.zipWithIndex.toMap
    new IndexMap(indices, elements)
  }

}