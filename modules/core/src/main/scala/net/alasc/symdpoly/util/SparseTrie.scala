package net.alasc.symdpoly
package util

import net.alasc.symdpoly.free.MutableWord
import net.alasc.symdpoly.math.Phase
import shapeless.Witness
import spire.syntax.cfor._
import spire.util.Opt

import scala.annotation.tailrec

/** A space efficient implementation of a trie, where only terminal nodes have a value. */
class SparseTrie[K, V](val bitset: Array[Long], val indices: Array[Int], val children: Array[AnyRef]) {

  def nChildren: Int = indices.length

  def child(index: Int): SparseTrie.Result[K, V] = {
    val w = index >>> 6
    val b = 1L << index
    if ((bitset(w) & b) == 0) SparseTrie.Result.empty[K, V] else {
      var arrayIndex = java.lang.Long.bitCount(bitset(w) & (b - 1))
      cforRange(0 until w) { v =>
        arrayIndex += java.lang.Long.bitCount(bitset(v))
      }
      new SparseTrie.Result[K, V](children(arrayIndex))
    }
  }

  /** Given a key, tries to match an element in this [[SparseTrie]] starting from the key position startPos.
    *
    * If there is a match, it returns the value of the match and sets the length of the match
    * at the first position in the array matchLengthOut.
    */
  def apply(key: K)(implicit K: SparseTrie.Key[K]): Opt[V] = {
    @tailrec def rec(branch: SparseTrie[K, V], pos: Int, length: Int): Opt[V] =
      if (pos >= length) Opt.empty[V] else branch.child(K.indexAt(key, pos)) match {
        case SparseTrie.IsLeaf(leaf) => Opt(leaf)
        case SparseTrie.IsBranch(newBranch) => rec(newBranch, pos + 1, length)
        case _ => Opt.empty[V]
      }
    rec(this, 0, K.length(key))
  }

  def entries(implicit K: SparseTrie.Key[K]): Seq[(K, V)] = {
    def collect(branch: SparseTrie[K, V]): Seq[(List[Int], V)] = {
      Seq.range(0, branch.nChildren).flatMap { i =>
        val index = branch.indices(i)
        new SparseTrie.Result[K, V](branch.children(i)) match {
          case SparseTrie.IsLeaf(value) => Seq((index :: Nil, value))
          case SparseTrie.IsBranch(newBranch) =>
            collect(newBranch).map {
              case (subindices, v) => (index :: subindices, v)
            }
        }
      }
    }
    collect(this).map {
      case (indices, v) => (SparseTrie.Key[K].fromIndices(indices.toArray), v)
    }
  }

}

object SparseTrie {

  protected def construct[K:Key, V](entries: Seq[(K, V)], pos: Int): SparseTrie[K, V] = {
    val bitset = new Array[Long]((Key[K].nIndices + 63) >> 6)
    val groups = entries.groupBy(kv => Key[K].indexAt(kv._1, pos))
    val indices = groups.keys.toArray.sorted
    val children = indices.map[AnyRef, Array[AnyRef]] { index =>
      bitset(index >>> 6) |= (1L << index)
      val subset = groups(index)
      subset.find(kv => Key[K].length(kv._1) - 1 == pos) match {
        case Some((_, value)) if value.isInstanceOf[SparseTrie[_, _]] => new Wrap[V](value)
        case Some((_, value)) => value.asInstanceOf[AnyRef]
        case None => construct(subset, pos + 1)
      }
    }
    new SparseTrie[K, V](bitset, indices, children)
  }

  def apply[K:Key, V](entries: (K, V)*): SparseTrie[K, V] = construct(entries, 0)

  protected[SparseTrie] object Empty {
    override def toString: String = "Empty"
  }

  protected[SparseTrie] case class Wrap[V](value: V) {
    override def toString: String = s"Wrap($value)"
  }

  /** Encapsulates the result of an `lookup` operation on a [[SparseTrie]].
    *
    * @param v Either contains
    *          - Empty representing a missing value
    *          - a [[SparseTrie]] representing a branch
    *          - a Wrap(value: SparseTrie) when the value is of type [[SparseTrie]] (this to avoid ambiguity)
    *          - a value: V when V is not of type [[SparseTrie]]
    */
  class Result[K, V](val a: AnyRef) extends AnyVal {
    override def toString: String = this match {
      case IsBranch(branch) => s"Branch($branch)"
      case IsLeaf(leaf) => s"Leaf($leaf)"
      case _ => Empty.toString
    }
    def asBranch: BranchResult[K, V] = a match {
      case st: SparseTrie[K, V] => new BranchResult[K, V](a)
      case _ => new BranchResult[K, V](Empty)
    }
    def asLeaf: LeafResult[V] = a match {
      case st: SparseTrie[K, V] => new LeafResult[V](Empty)
      case w: Wrap[V] => new LeafResult[V](w.value.asInstanceOf[AnyRef])
      case _ => new LeafResult[V](a)
    }
  }

  object Result {
    def empty[K, V]: Result[K, V] = new Result[K, V](Empty)
  }

  class BranchResult[K, V](val a: AnyRef) extends AnyVal {
    def isEmpty: Boolean = (a eq Empty)
    def get: SparseTrie[K, V] = a.asInstanceOf[SparseTrie[K, V]]
  }

  class LeafResult[V](val a: AnyRef) extends AnyVal {
    def isEmpty: Boolean = (a eq Empty)
    def get: V = a.asInstanceOf[V]
  }

  object IsBranch {
    def unapply[K, V](result: Result[K, V]): BranchResult[K, V] = result.asBranch
  }

  object IsLeaf {
    def unapply[K, V](result: Result[K, V]): LeafResult[V] = result.asLeaf
  }

  /** Describes a key used in a [[SparseTrie]], which is seen as a sequence of integers (i1,i2,...,i_length),
    * where each of i1, i2, ..., i_length takes a value between 0 and nIndices - 1. */
  trait Key[K] {

    /** Returns the number of index values. */
    def nIndices: Int

    /** Returns the number of indices in the given key. */
    def length(key: K): Int

    /** Returns the index at position "pos" in the given "key". */
    def indexAt(key: K, pos: Int): Int

    /** Reconstructs a key from its indices. */
    def fromIndices(indices: Array[Int]): K

  }

  object Key {
    final def apply[K](implicit ev: Key[K]): Key[K] = ev
  }

  /** Key strategy for String. */
  implicit object StringKey extends Key[String] {
    def nIndices: Int = 65536
    def length(key: String): Int = key.length
    def indexAt(key: String, pos: Int): Int = key(pos).toInt
    def fromIndices(indices: Array[Int]): String = indices.map(_.toChar).mkString
  }

  /** Key strategy for MutableWord (with immutable bit set): the zero monomial cannot be a key. */
  implicit def MutableWordKey[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux]: Key[MutableWord[F]] = new Key[MutableWord[F]] {
    def nIndices: Int = valueOf[F].nOperators
    def length(key: MutableWord[F]): Int = key.length
    def indexAt(key: MutableWord[F], pos: Int): Int = key.indices(pos)
    def fromIndices(indices: Array[Int]): MutableWord[F] = new MutableWord[F](Phase.one, indices.length, indices, false)
  }

}
