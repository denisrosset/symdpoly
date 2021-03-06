package net.alasc.symdpoly
package free

import cyclo.Cyclo
import spire.algebra._
import spire.syntax.cfor._

import net.alasc.perms.Perm
import net.alasc.symdpoly.math.{GenPerm, Phase, PhasedInt, Phases}
import shapeless.Witness
import spire.math.Rational
import spire.std.int._
import spire.compat._
import net.alasc.finite.Grp
import net.alasc.symdpoly.freebased.{Mono, Permutation, Poly}
import net.alasc.symdpoly.generic.{MonoLike, PolyLike}
import net.alasc.symdpoly.util.{IndexMap, OrderedSet, SparseTrie}
import net.alasc.symdpoly.quotient.{MonoDef => QuotientMonoidDef, Rules => QuotientRules}

/** Base class for a generalized free monoid.
  *
  * This class must be used as follows.
  *
  * - Declare inner case classes extending Op for each type of operator involved in the ring.
  * - Call declare for each operator with the relevant range of indices.
  * - Implement the abstract method "adjoint" for non hermitian operators.
  */
abstract class MonoDef(val cyclotomicOrder: Int) extends freebased.MonoDef {
  monoidDef =>

  import MonoDef.booleans

  //region Abstract members to implement, and members that can be overriden

  /** Sequence of all operator families appearing in this free monoid. */
  def families: Seq[OpFamily]

  /** Sequence of all operators of this free monoid. Override to prescribe a custom order. */
  def operators: Seq[Op] = families.flatMap(oc => oc.allInstances)

  //endregion

  //region Implementation of the (trivial) [[FreeBasedMonoidDef]] elements.

  type Free = monoidDef.type

  def Free: Free = this

  def quotient(word: Mono[Free, Free]): MonoType = word

  def quotient(poly: Poly[Free, Free]): Poly[Free, Free] = poly

  def inPlaceNormalForm(word: MutableWord[Free], start: Int): Boolean = false // normal form reduction is a no op

  //endregion

  //region Cached instances

  val mutableWordOrder: Order[MutableWord[Free]] = Order.from(_.compareTo(_))
  val immutableMutableWordOne: MutableWord[Free] = MutableWord.one[Free].setImmutable()
  val immutableMutableWordMinusOne: MutableWord[Free] = MutableWord.apply[Free](Phase.minusOne).setImmutable()
  val immutableMutableWordZero: MutableWord[Free] = MutableWord.zero[Free].setImmutable()

  implicit val phasedOpOrder: Order[PhasedOp[Free]] = new Order[PhasedOp[Free]] {
    def compare(x: PhasedOp[Free], y: PhasedOp[Free]): Int = Op.opOrder.compare(x.op, y.op) match {
      case 0 => Phase.order.compare(x.phase, y.phase)
      case i => i
    }
  }
  implicit val phasedOpGenPermAction: Action[PhasedOp[Free], GenPerm] = new Action[PhasedOp[Free], GenPerm] {
    def actl(g: GenPerm, p: PhasedOp[Free]): PhasedOp[Free] = actr(p, g.inverse)

    def actr(p: PhasedOp[Free], g: GenPerm): PhasedOp[Free] = {
      val PhasedOp(phase, op) = p
      val PhasedInt(newPhase, newIndex) = g.image(PhasedInt(phase, indexFromOp(op)))
      PhasedOp(newPhase, opFromIndex(newIndex))
    }
  }

  //endregion

  /** Symmetry group of this free monoid containing all generalized permutations of letters with all possible phases
    * up to the [[cyclotomicOrder]] of this free monoid. */
  def symmetryGroup: Grp[freebased.Permutation[monoidDef.type, monoidDef.type]] = {
    import net.alasc.perms.default._
    val grp = GenPerm.generalizedSymmetricGroup(cyclotomicOrder, nOperators)
    val generators = grp.generators.map(new freebased.Permutation[monoidDef.type, monoidDef.type](_))
    Grp.fromGeneratorsAndOrder(generators, grp.order)
  }

  //region Operator variables and handling of these

  lazy val opIndexMap: IndexMap[Op] = IndexMap(operators)

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
    * Instance types must be declared in this monoid [[families]] sequence before using them in monomials/polynomials.
    */
  abstract class Op extends Product with PhasedOpLike[Free] with MonoLike[Free] with PolyLike[Free] {
    lhs =>
    def M: Free = monoidDef

    def index: Int = indexFromOp(this)

    def toPhasedOp: PhasedOp[Free] = PhasedOp(Phase.one, lhs)
    def toMono: Mono[Free, Free] = Mono.fromOp[Free](lhs)
    def toPoly: Poly[Free, Free] = freebased.Poly(lhs.toMono)

    /** Method that, for each operator in this ring, returns its adjoint.
      *
      * For Hermitian operators, the adjoint operation is the identity.
      */
    def adjoint: Op
  }

  object Op {
    /** Order on operators. */
    implicit val opOrder: Order[Op] = Order.by(indexFromOp)
  }

  /** Abstract base class for Hermitian operator variables: implement a no op adjoint operation. */
  abstract class HermitianOp extends Op {
    selfOp =>
    def adjoint: Op = selfOp
  }

  /** A collection of operator variables; more general than a family as it can describe slices, i.e. subsets
    * of family elements.
    */
  sealed trait OpEnum {
    lhs =>
    def name: String

    def allInstances: Seq[Op]

    def ++(rhs: OpEnum): OpEnum = {
      import spire.compat._
      val unique = (lhs.allInstances.toSet union rhs.allInstances.toSet).toSeq.sorted
      OpEnum.fromSeq(s"$lhs ++ $rhs", unique)
    }
  }

  object OpEnum {
    /** Constructs a collection of operators from a set of operators and a name. */
    def fromSeq(name0: String, seq: Seq[Op]): OpEnum =
      new OpEnum {
        def name: String = name0

        def allInstances: Seq[Op] = seq
      }
  }

  /** Base trait for a family of operators, regrouping for example a sequence of operators indexed by 1, 2 or 3 integers.
    *
    * Usually implemented with the [[HermitianOp]], [[OpFamily1]], [[HermitianOpFamily1]], ...
    * subclasses.
    */
  trait OpFamily extends OpEnum {
    /** Name of this operator family. */
    def name: String = allInstances.headOption match {
      case Some(op) => op.productPrefix
      case _ => "{}"
    }

    /** Enumeration of all operators in this family of operators. Is provided automatically by the helper subclasses. */
    def allInstances: Seq[Op]
  }

  //endregion

  //region Base classes used to instantiate operator variables

  /** Base class for a singleton family containing one Hermitian operator.
    *
    * Used to instantiate single operator variables using the syntax
    *
    * case object X extends HermitianOp
    *
    * In that case, we do not distinguish between an operator instance and its family.
    */
  protected abstract class HermitianSingleOp extends HermitianOp with OpFamily {
    val allInstances = Seq(this)
  }

  /** Base class for the companion object of a family of Hermitian operators with one index. */
  protected abstract class OpFamily1(iSeq: Seq[Int]) extends OpFamily {
    val allInstances: Seq[Op] = for (i <- iSeq; a <- Seq(false, true)) yield apply(i, a)

    def apply(i: Int, adjoint: Boolean): Op

    protected def instances(si: Slice): Seq[Op] = for (i <- iSeq if si.contains(i); a <- booleans) yield apply(i, a)

    /** Returns the subset of operators in this family corresponding to the given slice. See [[Slice]]. */
    def slice(si: Slice): OpEnum = OpEnum.fromSeq(s"$name($si)", instances(si))
  }

  /** Base class for the companion object of a family of operators with one index. */
  protected abstract class HermitianOpFamily1(iSeq: Seq[Int]) extends OpFamily {
    val allInstances: Seq[Op] = iSeq.map(apply)

    def apply(i: Int): Op

    protected def instances(si: Slice): Seq[Op] = for (i <- iSeq if si.contains(i)) yield apply(i)

    /** Returns the subset of operators in this family corresponding to the given slice. See [[Slice]]. */
    def slice(si: Slice): OpEnum = OpEnum.fromSeq(s"$name($si)", instances(si))
  }

  /** Base class for the companion object of a family of operators with two indices. */
  protected abstract class OpFamily2(iSeq: Seq[Int], jSeq: Int => Seq[Int]) extends OpFamily {
    def this(iSeq: Seq[Int], jSeq: Seq[Int]) = this(iSeq, i => jSeq)

    val allInstances: Seq[Op] = for (i <- iSeq; j <- jSeq(i); a <- Seq(false, true)) yield apply(i, j, a)

    def apply(i: Int, j: Int, adjoint: Boolean): Op

    protected def instances(si: Slice, sj: Slice): Seq[Op] =
      for (i <- iSeq if si.contains(i); j <- jSeq(i) if sj.contains(j); a <- booleans) yield apply(i, j, a)

    /** Returns the subset of operators in this family corresponding to the given slices. See [[Slice]]. */
    def slice(si: Slice, sj: Slice): OpEnum = OpEnum.fromSeq(s"$name($si, $sj)", instances(si, sj))
  }

  /** Base class for the companion object of a family of Hermitian operators with two indices. */
  protected abstract class HermitianOpFamily2(iSeq: Seq[Int], jSeq: Int => Seq[Int]) extends OpFamily {
    def this(iSeq: Seq[Int], jSeq: Seq[Int]) = this(iSeq, i => jSeq)

    val allInstances: Seq[Op] = for (i <- iSeq; j <- jSeq(i)) yield apply(i, j)

    def apply(i: Int, j: Int): Op

    protected def instances(si: Slice, sj: Slice): Seq[Op] =
      for (i <- iSeq if si.contains(i); j <- jSeq(i) if sj.contains(j)) yield apply(i, j)

    /** Returns the subset of operators in this family corresponding to the given slices. See [[Slice]]. */
    def slice(si: Slice, sj: Slice): OpEnum = OpEnum.fromSeq(s"$name($si, $sj)", instances(si, sj))
  }

  /** Base class for the companion object of a family of operators with three indices. */
  protected abstract class OpFamily3(iSeq: Seq[Int], jSeq: Int => Seq[Int], kSeq: (Int, Int) => Seq[Int]) extends OpFamily {
    def this(iSeq: Seq[Int], jSeq: Seq[Int], kSeq: Seq[Int]) = this(iSeq, i => jSeq, (i, j) => kSeq)

    val allInstances: Seq[Op] = for (i <- iSeq; j <- jSeq(i); k <- kSeq(i, j); a <- Seq(false, true)) yield apply(i, j, k, a)

    def apply(i: Int, j: Int, k: Int, adjoint: Boolean): Op

    protected def instances(si: Slice, sj: Slice, sk: Slice): Seq[Op] =
      for (i <- iSeq if si.contains(i); j <- jSeq(i) if sj.contains(j); k <- kSeq(i, j) if sk.contains(k); a <- booleans) yield apply(i, j, k, a)

    /** Returns the subset of operators in this family corresponding to the given slices. See [[Slice]]. */
    def slice(si: Slice, sj: Slice, sk: Slice): OpEnum = OpEnum.fromSeq(s"$name($si, $sj, $sk)", instances(si, sj, sk))
  }

  /** Base class for the companion object of a family of Hermitian operators with three indices. */
  protected abstract class HermitianOpFamily3(iSeq: Seq[Int], jSeq: Int => Seq[Int], kSeq: (Int, Int) => Seq[Int]) extends OpFamily {
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

  /** Constructs a generalized permutation of the operator variables of this free monoid.
    *
    * @param f Image function
    */
  def permutation(f: Op => PhasedOp[this.type]): freebased.Permutation[this.type, this.type] = {
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
    new freebased.Permutation[this.type, this.type](GenPerm(perm, phases))
  }

  /** Constructs the quotient monoid of this free monoid, where the quotient monoid equivalence classes are given by confluent rewriting rules.
    *
    * @param rules Rewriting rules given as sequences of pairs (lhs -> rhs) describing substitutions.
    */
  def quotientMonoid(rules: QuotientRules[monoidDef.type]*): QuotientMonoidDef.Aux[monoidDef.type] = {
    val allRules = rules.flatMap(_.map { case (k, v) => (k.data, v.data) })
    val rewritingRules0 = SparseTrie[MutableWord[monoidDef.type], MutableWord[monoidDef.type]](allRules: _*)
    val maximalLhsLength0: Int = allRules.foldLeft(0) { case (mx, (lhs, rhs)) => spire.math.max(mx, lhs.length) }

    new QuotientMonoidDef {
      type Free = monoidDef.type
      def Free: Free = monoidDef
      val rewritingRules: SparseTrie[MutableWord[monoidDef.type], MutableWord[monoidDef.type]] =
        rewritingRules0
      val maximalLhsLength: Int = maximalLhsLength0
    }
  }

  //endregion


}

object MonoDef {
  val booleans = Seq(true, false)
  type Aux[F <: MonoDef with Singleton] = MonoDef { type Free = F }
}
