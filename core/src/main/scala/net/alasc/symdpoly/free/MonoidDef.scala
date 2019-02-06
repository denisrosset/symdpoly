package net.alasc.symdpoly
package free

import cyclo.Cyclo
import spire.algebra._
import spire.syntax.cfor._
import net.alasc.perms.Perm
import net.alasc.symdpoly
import net.alasc.symdpoly.generic.FreeBasedMonoidDef
import net.alasc.symdpoly.math.{GenPerm, PhasedInt, Phases}
import shapeless.Witness
import spire.math.Rational

class IndexMap[A](val index: Map[A, Int], val element: Seq[A]) {
  def size: Int = element.size
}

object IndexMap {
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
  * - Implement the abstract method "adjoint".
  */
abstract class MonoidDef extends FreeBasedMonoidDef {
  monoidDef =>

  import MonoidDef.booleans

  def operators: Seq[OpType]

  type Free = monoidDef.type
  def Free: Free = this

  // quotient morphism are trivial
  def quotient(word: Mono[Free, Free]): Monomial = word
  def quotient(poly: Poly[Free, Free]): Poly[Free, Free] = poly

  val immutableMutableWordOne = new MutableWord[Free](Phase.one, 0, new Array[Int](0), false)
  val immutableMutableWordMinusOne = new MutableWord[Free](Phase.minusOne, 0, new Array[Int](0), false)
  val immutableMutableWordZero = new MutableWord[Free](Phase.one, -1, new Array[Int](0), false)

  def inPlaceNormalForm(word: MutableWord[Free], start: Int): Boolean = false

  val mutableWordOrder: MutableWordOrder[Free] = new MutableWordOrder

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

  def nOperators: Int = opIndexMap.size
  def indexAdjoint(opIndex: Int): Int = adjointIndices(opIndex)

  def opFromIndex(i: Int): Op = opIndexMap.element(i)
  def indexFromOp(o: Op): Int = opIndexMap.index(o)

  /** Base class for operator variables.
    *
    * Instances must be declared using "declare" before using them in monomials/polynomials.
    */
  abstract class Op extends Product with MonoTerm[Free, Free] with PolyTerm[Free, Free] { lhs =>
    def wM: Witness.Aux[Free] = witnessFree
    def index: Int = indexFromOp(this)

    def toMono: Mono[Free, Free] = Mono.fromOp(lhs)
    def toPoly: Poly[Free, Free] = Poly(lhs.toMono)
    def +(rhs: Poly[Free, Free]): Poly[Free, Free] = lhs.toPoly + rhs
    def *(rhs: Poly[Free, Free]): Poly[Free, Free] = lhs.toPoly * rhs
    def *(rhs: Mono[Free, Free])(implicit mm: MultiplicativeMonoid[Mono[Free, Free]]): Mono[Free, Free] = lhs.toMono * rhs

    // Returns PhasedOp
    def unary_- : PhasedOp = lhs * Phase.minusOne
    def *(rhs: Phase): PhasedOp = PhasedOp(rhs, lhs)

    /** Method that, for each operator in this ring, returns its adjoint.
      *
      * For Hermitian operators, the adjoint operation corresponds to the identity.
      */
    def adjoint: Op
  }

  abstract class HermitianOp extends Op { selfOp =>
    def adjoint: Op = selfOp
  }

  sealed trait OpEnum {
    def name: String
    def allInstances: Seq[Op]
  }

  object OpEnum {
    def fromSeq(name0: String, seq: Seq[Op]): OpEnum =
      new OpEnum {
        def name: String = name0
        def allInstances: Seq[Op] = seq
      }
  }

  trait OpType extends OpEnum {
    def name: String = allInstances match {
      case Seq(hd, _*) => hd.productPrefix
      case _ => "{}"
    }
    def allInstances: Seq[Op]
  }

  abstract class HermitianType1(iSeq: Seq[Int]) extends OpType {
    val allInstances: Seq[Op] = iSeq.map(apply)
    def apply(i: Int): Op
    protected def instances(si: Slice): Seq[Op] = for (i <- iSeq if si.contains(i)) yield apply(i)
    def slice(si: Slice): OpEnum = OpEnum.fromSeq(s"$name($si)", instances(si))
  }

  abstract class NonHermitianType1(iSeq: Seq[Int]) extends OpType {
    val allInstances: Seq[Op] = for (i <- iSeq; a <- Seq(false, true)) yield apply(i, a)
    def apply(i: Int, adjoint: Boolean): Op
    protected def instances(si: Slice): Seq[Op] = for (i <- iSeq if si.contains(i); a <- booleans) yield apply(i, a)
    def slice(si: Slice): OpEnum = OpEnum.fromSeq(s"$name($si)", instances(si))
  }

  abstract class HermitianType2(iSeq: Seq[Int], jSeq: Int => Seq[Int]) extends OpType {
    def this(iSeq: Seq[Int], jSeq: Seq[Int]) = this(iSeq, i => jSeq)
    val allInstances: Seq[Op] = for (i <- iSeq; j <- jSeq(i)) yield apply(i, j)
    def apply(i: Int, j: Int): Op
    protected def instances(si: Slice, sj: Slice): Seq[Op] =
      for (i <- iSeq if si.contains(i); j <- jSeq(i) if sj.contains(j)) yield apply(i, j)
    def slice(si: Slice, sj: Slice): OpEnum = OpEnum.fromSeq(s"$name($si, $sj)", instances(si, sj))
  }

  abstract class NonHermitianType2(iSeq: Seq[Int], jSeq: Int => Seq[Int]) extends OpType {
    def this(iSeq: Seq[Int], jSeq: Seq[Int]) = this(iSeq, i => jSeq)
    val allInstances: Seq[Op] = for (i <- iSeq; j <- jSeq(i); a <- Seq(false, true)) yield apply(i, j, a)
    def apply(i: Int, j: Int, adjoint: Boolean): Op
    protected def instances(si: Slice, sj: Slice): Seq[Op] =
      for (i <- iSeq if si.contains(i); j <- jSeq(i) if sj.contains(j); a <- booleans) yield apply(i, j, a)
    def slice(si: Slice, sj: Slice): OpEnum = OpEnum.fromSeq(s"$name($si, $sj)", instances(si, sj))
  }

  abstract class HermitianType3(iSeq: Seq[Int], jSeq: Int => Seq[Int], kSeq: (Int, Int) => Seq[Int]) extends OpType {
    def this(iSeq: Seq[Int], jSeq: Seq[Int], kSeq: Seq[Int]) = this(iSeq, i => jSeq, (i, j) => kSeq)
    val allInstances: Seq[Op] = for (i <- iSeq; j <- jSeq(i); k <- kSeq(i, j)) yield apply(i, j, k)
    def apply(i: Int, j: Int, k: Int): Op
    protected def instances(si: Slice, sj: Slice, sk: Slice): Seq[Op] =
      for (i <- iSeq if si.contains(i); j <- jSeq(i) if sj.contains(j); k <- kSeq(i, j) if sk.contains(k)) yield apply(i, j, k)
    def slice(si: Slice, sj: Slice, sk: Slice): OpEnum = OpEnum.fromSeq(s"$name($si, $sj, $sk)", instances(si, sj, sk))
  }

  abstract class NonHermitianType3(iSeq: Seq[Int], jSeq: Int => Seq[Int], kSeq: (Int, Int) => Seq[Int]) extends OpType {
    def this(iSeq: Seq[Int], jSeq: Seq[Int], kSeq: Seq[Int]) = this(iSeq, i => jSeq, (i, j) => kSeq)
    val allInstances: Seq[Op] = for (i <- iSeq; j <- jSeq(i); k <- kSeq(i, j); a <- Seq(false, true)) yield apply(i, j, k, a)
    def apply(i: Int, j: Int, k: Int, adjoint: Boolean): Op
    protected def instances(si: Slice, sj: Slice, sk: Slice): Seq[Op] =
      for (i <- iSeq if si.contains(i); j <- jSeq(i) if sj.contains(j); k <- kSeq(i, j) if sk.contains(k); a <- booleans) yield apply(i, j, k, a)
    def slice(si: Slice, sj: Slice, sk: Slice): OpEnum = OpEnum.fromSeq(s"$name($si, $sj, $sk)", instances(si, sj, sk))
  }

  case class PhasedOp(phase: Phase, op: Op) extends MonoTerm[Free, Free] with PolyTerm[Free, Free] { lhs =>
    override def toString: String = Mono[Free](phase, op).toString
    def toPoly: Poly[Free, Free] = Poly(lhs.toMono)
    def toMono: Mono[Free, Free] = Mono(lhs)
    def +(rhs: Poly[Free, Free]): Poly[Free, Free] = lhs.toPoly + rhs
    def *(rhs: Poly[Free, Free]): Poly[Free, Free] = lhs.toPoly * rhs
    def *(rhs: Mono[Free, Free])(implicit mm: MultiplicativeMonoid[Mono[Free, Free]]): Mono[Free, Free] = lhs.toMono * rhs
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

  def generator(f: Op => PhasedOp)(implicit name: sourcecode.Name): Generator[this.type] = {
    import scala.collection.mutable.{HashMap => MMap}
    val phaseMap: MMap[Int, Phase] = MMap.empty[Int, Phase]
    val permImages = new Array[Int](nOperators)
    cforRange(0 until nOperators) { i =>
      val PhasedOp(newPhase, newOp) = f(opFromIndex(i))
      val newI = indexFromOp(newOp)
      phaseMap(newI) = newPhase
      permImages(i) = newI
    }
    val perm = Perm.fromImages(permImages)
    val phases = Phases(phaseMap.toVector: _*)
    new Generator[Free](name.value, GenPerm(perm, phases))
  }

  def permutation(f: Op => PhasedOp): OpPermutation[this.type] = {
    import scala.collection.mutable.{HashMap => MMap}
    val phaseMap: MMap[Int, Phase] = MMap.empty[Int, Phase]
    val permImages = new Array[Int](nOperators)
    cforRange(0 until nOperators) { i =>
      val PhasedOp(newPhase, newOp) = f(opFromIndex(i))
      val newI = indexFromOp(newOp)
      phaseMap(newI) = newPhase
      permImages(i) = newI
    }
    val perm = Perm.fromImages(permImages)
    val phases = Phases(phaseMap.toVector: _*)
    new OpPermutation[this.type](GenPerm(perm, phases))
  }

}

object MonoidDef {
  val booleans = Seq(true, false)
  type Aux[F <: MonoidDef with Singleton] = MonoidDef { type Free = F }
}
