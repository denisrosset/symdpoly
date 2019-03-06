package net.alasc.symdpoly
package quotient
import net.alasc.algebra.PermutationAction
import net.alasc.symdpoly.free.{MutablePoly, MutableWord}
import net.alasc.symdpoly.generic.{FreeBasedMono, FreeBasedMonoidDef, FreeBasedPermutation}
import spire.syntax.cfor._

import scala.annotation.tailrec
import net.alasc.bsgs.UnorderedPartitionStabilizer
import net.alasc.finite.Grp
import net.alasc.partitions.Partition
import net.alasc.symdpoly
import net.alasc.symdpoly.Phase
import net.alasc.symdpoly.math.GenPerm
import net.alasc.util._
import spire.syntax.eq._
import cats.instances.vector._
import spire.syntax.group._
import cats.instances.option._
import cats.syntax.traverse._
import shapeless.Witness
import spire.util.Opt

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

/** Base class for quotient monoids. */
abstract class MonoidDef extends FreeBasedMonoidDef {
  monoidDef =>

  def cyclotomicOrder: Int = Free.cyclotomicOrder

  def rewritingRules: SparseTrie[MutableWord[Free], MutableWord[Free]]

  def maximalLhsLength: Int

  /** Helper to describe the symmetries that are compatible with the quotient monoid. */
  object Symmetries {

    private[this] val m = Free.cyclotomicOrder
    private[this] val n = Free.nOperators
    private[this] val phases: Vector[Phase] = Vector.tabulate(m)(k => Phase(k, m))
    private[this] def op(i: Int): Free#Op = Free.opFromIndex(i)
    private[this] def monoFromOpIndex(i: Int): FreeBasedMono[Free, Free] = FreeBasedMono(op(i))

    /** Ordered set of all monomials of degree <= 2, with all possible phases. */
    private[this] val monoSet: OrderedSet[FreeBasedMono[Free, Free]] = {
      val monos1: Vector[FreeBasedMono[Free, Free]] = Vector.tabulate(n)(i => monoFromOpIndex(i))
      val monos2: Vector[FreeBasedMono[Free, Free]] = Vector.tabulate(n, n)((i, j) => FreeBasedMono(op(i), op(j)) ).flatten
      val monos: Vector[FreeBasedMono[Free, Free]] = Vector(FreeBasedMono.one[Free, Free]) ++ (monos1 ++ monos2).flatMap(m => phases.map(p => m * p))
      OrderedSet.fromUnique(monos)
    }

    /** Permutation action of free permutations on the set of monomials described by [[monoSet]] . */
    val action = new PermutationAction[FreeBasedPermutation[Free, Free]] {
      def isFaithful: Boolean = true
      def findMovedPoint(g: FreeBasedPermutation[Free, Free]): NNOption = g.genPerm.largestMovedPoint match {
        case NNOption(i) => NNSome(monoSet.indexOf(monoFromOpIndex(i)))
        case _ => NNNone
      }
      def movedPointsUpperBound(g: FreeBasedPermutation[Free, Free]): NNOption = NNSome(monoSet.length - 1)
      def actl(g: FreeBasedPermutation[Free, Free], i: Int): Int = actr(i, g.inverse)
      def actr(i: Int, g: FreeBasedPermutation[Free, Free]): Int = monoSet.indexOf(Free.monoGenPermAction.actr(monoSet(i), g.genPerm)) // TODO: replace
    }

    /** Partition given by equivalent monomials in [[monoSet]]. */
    val partition = {
      val normalForms = monoSet.iterator.map(monoidDef.quotient(_)).toVector
      Partition.fromSeq(normalForms)
    }

  }

  /** Returns the subgroup of a group of permutations on the free variables, such that it is the maximal subgroup
    * compatible with the quotient structure. */
  def groupInQuotient(grp: Grp[FreeBasedPermutation[Free, Free]]): Grp[Permutation] = {
    import net.alasc.perms.default._
    grp.generators.toVector.map(quotient).sequence match {
      case Some(mappedGenerators) => Grp.fromGeneratorsAndOrder(mappedGenerators, grp.order)
      case None => groupInQuotient(grp.unorderedPartitionStabilizer(Symmetries.action, Symmetries.partition))
    }
  }

  /** Translates a group acting on the free variables into a group acting on the equivalence classes on the quotient
    * monoid, assuming that the group is compatible without verification. */
  def groupInQuotientNC(grp: Grp[FreeBasedPermutation[Free, Free]]): Grp[Permutation] = {
    import net.alasc.perms.default._
    Grp.fromGeneratorsAndOrder(grp.generators.map(quotientNC), grp.order)
  }

  /** Symmetry group that preserves the quotient structure, with elements that act by permuting operator variables,
    * possibly applying a phase. */
  lazy val symmetryGroup: Grp[Permutation] = groupInQuotient(Free.symmetryGroup)

  /** Returns the permutation of the quotient monoid equivalence classes that correspond to the free permutation given,
    * without performing sanity checks. */
  def quotientNC(permutation: FreeBasedPermutation[Free, Free]): FreeBasedPermutation[monoidDef.type, Free] =
    new FreeBasedPermutation[monoidDef.type, Free](permutation.genPerm)

  /** Returns the permutation of the quotient monoid equivalence classes that correspond to the given free permutation
    * when it is compatible, or returns None otherwise.
    */
  def quotient(permutation: FreeBasedPermutation[Free, Free]): Option[FreeBasedPermutation[monoidDef.type, Free]] =
    if (UnorderedPartitionStabilizer.partitionInvariantUnder(Symmetries.partition, Symmetries.action, permutation))
      Some(new FreeBasedPermutation[monoidDef.type, Free](permutation.genPerm))
    else None

  /** Returns the representative class of the given free monomial. */
  def quotient(word: FreeBasedMono[Free, Free]): Monomial = {
    val res = word.data.mutableCopy()
    inPlaceNormalForm(res)
    new FreeBasedMono[monoidDef.type, Free](res.setImmutable())
  }

  /** Returns the representative class of the given free polynomial. */
  def quotient(poly: Poly[Free, Free]): Poly[monoidDef.type, Free] =
    if (poly.nTerms == 0) symdpoly.Poly.zero[monoidDef.type, Free] else {
      val res = MutablePoly.empty[Free](poly.nTerms)
      cforRange(0 until poly.nTerms) { i =>
        val newMono = poly.monomialNormalForm(i).mutableCopy()
        inPlaceNormalForm(newMono)
        val newCoeff = poly.coeff(i) * newMono.phase.toCyclo
        newMono.setPhase(Phase.one)
        res.add(newMono.setImmutable(), newCoeff)
      }
      res.immutableCopy[monoidDef.type]
    }

  /** Rewrites the given word in the free variables to its normal form in the quotient monoid.
    * Returns whether any rule application has taken place (even if the rule applications left the word
    * invariant in the end).
    */
  def inPlaceNormalForm(word: MutableWord[Free], start: Int = 0): Boolean =
    if (word.isZero) false else {
      @tailrec def rec(i: Int, modified: Boolean): Boolean =
        if (i < 0)
          rec(0, modified)
        else if (i >= word.length)
          modified
        else {
          /** Tries to find a match from indices i to j and returns whether a rewriting occurred. */
          @tailrec def findMatch(j: Int, branch: SparseTrie[MutableWord[Free], MutableWord[Free]]): Boolean =
            if (j == word.length) false else branch.child(word.indices(j)) match {
              case SparseTrie.IsBranch(newBranch) => findMatch(j + 1, newBranch)
              case SparseTrie.IsLeaf(value) =>
                word.replaceRange(i, j + 1, value)
                true
              case _ => false
            }
          if (findMatch(i, rewritingRules))
            rec(i - maximalLhsLength + 1, true)
          else
            rec(i + 1, modified)
        }
      rec(0, false)
    }

  /*
    def inPlaceNormalForm(word: MutableWord[Free], start: Int = 0): Boolean =
    if (word.isZero) false else {
      @tailrec def rec(i: Int, n: Int, modified: Boolean): Boolean =
        if (i < 0)
          rec(0, n, modified)
        else if (i >= n - 1) {
          word.length = n
          modified
        } else {
          val tailSize = n - i - 2
          val oi1 = word.indices(i)
          val oi2 = word.indices(i + 1)
          pairRules.rule(oi1, oi2) match {
            case PairRules.RemoveBoth => // discard x_i and x_i+1
              Array.copy(word.indices, i + 2, word.indices, i, tailSize) // move back two places the tail elements
              rec(i - 1, n - 2, true) // go back one element to check for possible new substitutions
            case PairRules.KeepFirst =>
              Array.copy(word.indices, i + 2, word.indices, i + 1, tailSize) // move back one place the tail elements
              rec(i, n - 1, true)
            case PairRules.Swap =>
              word.swap(i, i + 1) // swap x_i and x_i+1
              rec(i - 1, n, true) // go back one element to check for possible new substitutions
            case PairRules.SetToZero =>
              // the monomial was not zero before
              word.setToZero()
              true
            case PairRules.Preserve => rec(i + 1, n, modified) // both elements are good, move to next
            case PairRules.Custom =>
              val result = pairRules.custom(Free.opFromIndex(oi1) -> Free.opFromIndex(oi2))
              if (result.isZero) {
                word.setToZero()
                true
              } else {
                word *= result.phase // multiply phase
                result.length match {
                  case 0 => // as in PairRules.RemoveBoth
                    Array.copy(word.indices, i + 2, word.indices, i, tailSize) // move back two places
                    rec(i - 1, n - 2, true)
                  case 1 =>
                    Array.copy(word.indices, i + 2, word.indices, i + 1, tailSize)
                    word.indices(i) = result.data.indices(0)
                    rec(i - 1, n - 1, true)
                  case 2 =>
                    word.indices(i) = result.data.indices(0)
                    word.indices(i + 1) = result.data.indices(1)
                    rec(i - 1, n, true)
                  case _ =>
                    throw new IllegalArgumentException(s"Rules cannot grow the word size")
                }

              }
          }
        }
      rec(start, word.length, false)
    }

   */
}

object MonoidDef {

  type Aux[F <: free.MonoidDef with Singleton] = quotient.MonoidDef { type Free = F }

  /** Constructs a quotient monoid on the given free monoid by substitution rules that apply to pairs of operators. */
  def deprecatedApply[F <: free.MonoidDef.Aux[F] with Singleton](f: F)(pairSubstitutions: PairSubstitutions[F]): quotient.MonoidDef.Aux[F] = {
    implicit def witnessF: Witness.Aux[F] = (f: F).witness
    /** List of substitution rules that provide the normal form. */
    val rules: SparseTrie[MutableWord[F], MutableWord[F]] = {
      val list: Seq[(MutableWord[F], MutableWord[F])] = for {
        op1 <- (f: F).opIndexMap.elements
        op2 <- (f: F).opIndexMap.elements
        lhs = op1.toMono * op2
        rhs = pairSubstitutions.apply(op1, op2) if lhs =!= rhs
      } yield (lhs.data -> rhs.data)
      SparseTrie(list: _*)
    }
    new MonoidDef {
      type Free = F
      def Free: F = f
      val rewritingRules: SparseTrie[MutableWord[F], MutableWord[F]] = rules
      def maximalLhsLength: Int = 2
    }
  }

}
