package net.alasc.symdpoly
package free

import cyclo.Cyclo
import spire.algebra._
import spire.syntax.cfor._

import net.alasc.perms.Perm
import net.alasc.symdpoly
import net.alasc.symdpoly.generic.{FreeBasedMono, FreeBasedMonoTerm, FreeBasedMonoidDef, FreeBasedPermutation}
import net.alasc.symdpoly.math.{GenPerm, PhasedInt, Phases}
import shapeless.Witness
import spire.math.Rational

import net.alasc.finite.Grp

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

/** Base class for a generalized free monoid.
  *
  * This class must be used as follows.
  *
  * - Declare inner case classes extending Op for each type of operator involved in the ring.
  * - Call declare for each operator with the relevant range of indices.
  * - Implement the abstract method "adjoint" for non hermitian operators.
  */
abstract class MonoidDef(val cyclotomicOrder: Int) extends FreeBasedMonoidDef {
  monoidDef =>

  import MonoidDef.booleans

  //region Abstract members to implement

  /** Sequence of all operator types appearing in this free monoid. */
  def operators: Seq[OpFamily]

  //endregion

  //region Implementation of the (trivial) [[FreeBasedMonoidDef]] elements.

  type Free = monoidDef.type
  def Free: Free = this

  def quotient(word: FreeBasedMono[Free, Free]): Monomial = word
  def quotient(poly: Poly[Free, Free]): Poly[Free, Free] = poly

  def inPlaceNormalForm(word: MutableWord[Free], start: Int): Boolean = false // normal form reduction is a no op

  //endregion

  //region Cached instances

  val mutableWordOrder: MutableWordOrder[Free] = new MutableWordOrder
  val immutableMutableWordOne = new MutableWord[Free](Phase.one, 0, new Array[Int](0), false)
  val immutableMutableWordMinusOne = new MutableWord[Free](Phase.minusOne, 0, new Array[Int](0), false)
  val immutableMutableWordZero = new MutableWord[Free](Phase.one, -1, new Array[Int](0), false)

  //endregion

  /** Symmetry group of this free monoid containing all generalized permutations of letters with all possible phases
    * up to the [[cyclotomicOrder]] of this free monoid. */
  def symmetryGroup: Grp[FreeBasedPermutation[monoidDef.type, monoidDef.type]] = {
    import net.alasc.perms.default._
    val grp = GenPerm.generalizedSymmetricGroup(cyclotomicOrder, nOperators)
    val generators = grp.generators.map(new FreeBasedPermutation[monoidDef.type, monoidDef.type](_))
    Grp.fromGeneratorsAndOrder(generators, grp.order)
  }

  //region Operator variables and handling of these

  lazy val opIndexMap: IndexMap[Op] = IndexMap(operators.flatMap(oc => oc.allInstances))

  // Internally, operators are represented by their index
  private[this] lazy val adjointIndices: Array[Int] = {
    val indices = new Array[Int](nOperators)
    cforRange(0 until nOperators) { i =>
      val op = opFromIndex(i).adjoint
      indices(i) = indexFromOp(op)
    }
    indices
  }

  /** Returns the number of operator variables in this free monomial monoid. */
  def nOperators: Int = opIndexMap.size

  /** For the operator of index opIndex, returns the index of its adjoint. */
  def indexAdjoint(opIndex: Int): Int = adjointIndices(opIndex)

  /** Returns the i-th operator. */
  def opFromIndex(i: Int): Op = opIndexMap.elements(i)

  /** Returns the index of the given operator. */
  def indexFromOp(o: Op): Int = opIndexMap.indexMap(o)

  /** Abstract base class for operator variables.
    *
    * Instance types must be declared in this monoid [[operators]] sequence before using them in monomials/polynomials.
    */
  abstract class Op extends Product with FreeBasedMonoTerm[Free, Free] with PolyTerm[Free, Free] { lhs =>
    def wM: Witness.Aux[Free] = witnessFree
    def index: Int = indexFromOp(this)

    def toMono: FreeBasedMono[Free, Free] = FreeBasedMono.fromOp(lhs)
    def toPoly: Poly[Free, Free] = Poly(lhs.toMono)
    def +(rhs: Poly[Free, Free]): Poly[Free, Free] = lhs.toPoly + rhs
    def *(rhs: Poly[Free, Free]): Poly[Free, Free] = lhs.toPoly * rhs
    def *(rhs: FreeBasedMono[Free, Free])(implicit mm: MultiplicativeMonoid[FreeBasedMono[Free, Free]]): FreeBasedMono[Free, Free] = lhs.toMono * rhs

    // Returns PhasedOp
    def unary_- : PhasedOp = lhs * Phase.minusOne
    def *(rhs: Phase): PhasedOp = PhasedOp(rhs, lhs)

    /** Method that, for each operator in this ring, returns its adjoint.
      *
      * For Hermitian operators, the adjoint operation is the identity.
      */
    def adjoint: Op
  }

  /** Abstract base class for Hermitian operator variables: implement a no op adjoint operation. */
  abstract class HermitianOp extends Op { selfOp =>
    def adjoint: Op = selfOp
  }

  /** An operator variable in this free monoid along with a phase. */
  case class PhasedOp(phase: Phase, op: Op) extends FreeBasedMonoTerm[Free, Free] with PolyTerm[Free, Free] { lhs =>
    override def toString: String = FreeBasedMono[Free](phase, op).toString
    def toPoly: Poly[Free, Free] = Poly(lhs.toMono)
    def toMono: FreeBasedMono[Free, Free] = FreeBasedMono(lhs)
    def unary_- : PhasedOp = PhasedOp(-phase, op)
    def *(newPhase: Phase): PhasedOp = PhasedOp(phase * newPhase, op)
    def +(rhs: Poly[Free, Free]): Poly[Free, Free] = lhs.toPoly + rhs
    def *(rhs: Poly[Free, Free]): Poly[Free, Free] = lhs.toPoly * rhs
    def *(rhs: FreeBasedMono[Free, Free])(implicit mm: MultiplicativeMonoid[FreeBasedMono[Free, Free]]): FreeBasedMono[Free, Free] = lhs.toMono * rhs
  }

  object PhasedOp {
    implicit def fromOp(op: Op): PhasedOp = PhasedOp(Phase.one, op)
    implicit val phasedOpGenPermAction: Action[PhasedOp, GenPerm] = new Action[PhasedOp, GenPerm] {
      def actl(g: GenPerm, p: PhasedOp): PhasedOp = actr(p, g.inverse)
      def actr(p: PhasedOp, g: GenPerm): PhasedOp = {
        val PhasedOp(phase, op) = p
        val PhasedInt(newPhase, newIndex) = g.image(PhasedInt(phase, indexFromOp(op)))
        PhasedOp(newPhase, opFromIndex(newIndex))
      }
    }
  }

  /** A collection of operator variables; more general than a family as it can describe slices, i.e. subsets
    * of family elements.
    */
  sealed trait OpEnum {
    def name: String
    def allInstances: Seq[Op]
  }

  object OpEnum {
    /** Constructs a collection of operators from a sequence and a name. */
    def fromSeq(name0: String, seq: Seq[Op]): OpEnum =
      new OpEnum {
        def name: String = name0
        def allInstances: Seq[Op] = seq
      }
  }

  /** Base trait for a family of operators, regrouping for example a sequence of operators indexed by 1, 2 or 3 integers.
    *
    * Usually implemented with the [[SingleOp]], [[HermitianOp]], [[OpFamily1]], [[HermitianOpFamily1]], ...
    * subclasses.
    */
  trait OpFamily extends OpEnum {
    /** Name of this operator family. */
    def name: String = allInstances match {
      case Seq(hd, _*) => hd.productPrefix
      case _ => "{}"
    }
    /** Enumeration of all operators in this family of operators. Is provided automatically by the helper subclasses. */
    def allInstances: Seq[Op]
  }

  //endregion

  //region Base classes used to instantiate operator variables

  /** Base class for a singleton family containing one operator.
    *
    * Used to instantiate single operator variables using the syntax
    *
    *   case object X extends SingleOp
    *
    * In that case, we do not distinguish between an operator instance and its family.
    */
  abstract class SingleOp extends Op with OpFamily {
    val allInstances = Seq(this)
  }

  /** Base class for a singleton family containing one Hermitian operator.
    *
    * Used to instantiate single operator variables using the syntax
    *
    *   case object X extends HermitianOp
    *
    * In that case, we do not distinguish between an operator instance and its family.
    */
  abstract class HermitianSingleOp extends HermitianOp with OpFamily {
    val allInstances = Seq(this)
  }

  /** Base class for the companion object of a family of Hermitian operators with one index. */
  abstract class OpFamily1(iSeq: Seq[Int]) extends OpFamily {
    val allInstances: Seq[Op] = for (i <- iSeq; a <- Seq(false, true)) yield apply(i, a)
    def apply(i: Int, adjoint: Boolean): Op
    protected def instances(si: Slice): Seq[Op] = for (i <- iSeq if si.contains(i); a <- booleans) yield apply(i, a)

    /** Returns the subset of operators in this family corresponding to the given slice. See [[Slice]]. */
    def slice(si: Slice): OpEnum = OpEnum.fromSeq(s"$name($si)", instances(si))
  }

  /** Base class for the companion object of a family of operators with one index. */
  abstract class HermitianOpFamily1(iSeq: Seq[Int]) extends OpFamily {
    val allInstances: Seq[Op] = iSeq.map(apply)
    def apply(i: Int): Op
    protected def instances(si: Slice): Seq[Op] = for (i <- iSeq if si.contains(i)) yield apply(i)

    /** Returns the subset of operators in this family corresponding to the given slice. See [[Slice]]. */
    def slice(si: Slice): OpEnum = OpEnum.fromSeq(s"$name($si)", instances(si))
  }

  /** Base class for the companion object of a family of operators with two indices. */
  abstract class OpFamily2(iSeq: Seq[Int], jSeq: Int => Seq[Int]) extends OpFamily {
    def this(iSeq: Seq[Int], jSeq: Seq[Int]) = this(iSeq, i => jSeq)
    val allInstances: Seq[Op] = for (i <- iSeq; j <- jSeq(i); a <- Seq(false, true)) yield apply(i, j, a)
    def apply(i: Int, j: Int, adjoint: Boolean): Op
    protected def instances(si: Slice, sj: Slice): Seq[Op] =
      for (i <- iSeq if si.contains(i); j <- jSeq(i) if sj.contains(j); a <- booleans) yield apply(i, j, a)

    /** Returns the subset of operators in this family corresponding to the given slices. See [[Slice]]. */
    def slice(si: Slice, sj: Slice): OpEnum = OpEnum.fromSeq(s"$name($si, $sj)", instances(si, sj))
  }

  /** Base class for the companion object of a family of Hermitian operators with two indices. */
  abstract class HermitianOpFamily2(iSeq: Seq[Int], jSeq: Int => Seq[Int]) extends OpFamily {
    def this(iSeq: Seq[Int], jSeq: Seq[Int]) = this(iSeq, i => jSeq)
    val allInstances: Seq[Op] = for (i <- iSeq; j <- jSeq(i)) yield apply(i, j)
    def apply(i: Int, j: Int): Op
    protected def instances(si: Slice, sj: Slice): Seq[Op] =
      for (i <- iSeq if si.contains(i); j <- jSeq(i) if sj.contains(j)) yield apply(i, j)

    /** Returns the subset of operators in this family corresponding to the given slices. See [[Slice]]. */
    def slice(si: Slice, sj: Slice): OpEnum = OpEnum.fromSeq(s"$name($si, $sj)", instances(si, sj))
  }

  /** Base class for the companion object of a family of operators with three indices. */
  abstract class OpFamily3(iSeq: Seq[Int], jSeq: Int => Seq[Int], kSeq: (Int, Int) => Seq[Int]) extends OpFamily {
    def this(iSeq: Seq[Int], jSeq: Seq[Int], kSeq: Seq[Int]) = this(iSeq, i => jSeq, (i, j) => kSeq)
    val allInstances: Seq[Op] = for (i <- iSeq; j <- jSeq(i); k <- kSeq(i, j); a <- Seq(false, true)) yield apply(i, j, k, a)
    def apply(i: Int, j: Int, k: Int, adjoint: Boolean): Op
    protected def instances(si: Slice, sj: Slice, sk: Slice): Seq[Op] =
      for (i <- iSeq if si.contains(i); j <- jSeq(i) if sj.contains(j); k <- kSeq(i, j) if sk.contains(k); a <- booleans) yield apply(i, j, k, a)

    /** Returns the subset of operators in this family corresponding to the given slices. See [[Slice]]. */
    def slice(si: Slice, sj: Slice, sk: Slice): OpEnum = OpEnum.fromSeq(s"$name($si, $sj, $sk)", instances(si, sj, sk))
  }

  /** Base class for the companion object of a family of Hermitian operators with three indices. */
  abstract class HermitianOpFamily3(iSeq: Seq[Int], jSeq: Int => Seq[Int], kSeq: (Int, Int) => Seq[Int]) extends OpFamily {
    def this(iSeq: Seq[Int], jSeq: Seq[Int], kSeq: Seq[Int]) = this(iSeq, i => jSeq, (i, j) => kSeq)
    val allInstances: Seq[Op] = for (i <- iSeq; j <- jSeq(i); k <- kSeq(i, j)) yield apply(i, j, k)
    def apply(i: Int, j: Int, k: Int): Op
    protected def instances(si: Slice, sj: Slice, sk: Slice): Seq[Op] =
      for (i <- iSeq if si.contains(i); j <- jSeq(i) if sj.contains(j); k <- kSeq(i, j) if sk.contains(k)) yield apply(i, j, k)

    /** Returns the subset of operators in this family corresponding to the given slices. See [[Slice]]. */
    def slice(si: Slice, sj: Slice, sk: Slice): OpEnum = OpEnum.fromSeq(s"$name($si, $sj, $sk)", instances(si, sj, sk))
  }

  //endregion

  //region Construction of objects derived from this free monoid.

  def permutation(f: Op => PhasedOp): FreeBasedPermutation[this.type, this.type] = {
    import scala.collection.mutable.{HashMap => MMap}
    val phaseMap: MMap[Int, Phase] = MMap.empty[Int, Phase]
    val permImages = new Array[Int](nOperators)
    cforRange(0 until nOperators) { i =>
      val PhasedOp(newPhase, newOp) = f(opFromIndex(i))
      assert(cyclotomicOrder % newPhase.n == 0, s"Phase $newPhase for operator $newOp incompatible with declared cyclotomic order $cyclotomicOrder")
      val newI = indexFromOp(newOp)
      phaseMap(newI) = newPhase
      permImages(i) = newI
    }
    val perm = Perm.fromImages(permImages)
    val phases = Phases(phaseMap.toVector: _*)
    new FreeBasedPermutation[this.type, this.type](GenPerm(perm, phases))
  }

  //endregion

}

object MonoidDef {
  val booleans = Seq(true, false)
  type Aux[F <: MonoidDef with Singleton] = MonoidDef { type Free = F }
}
