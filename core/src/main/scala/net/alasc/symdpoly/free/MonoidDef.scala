package net.alasc.symdpoly
package free

import spire.algebra._
import spire.syntax.cfor._

import net.alasc.perms.Perm
import net.alasc.symdpoly
import net.alasc.symdpoly.generic.FreeBasedMonoidDef
import net.alasc.symdpoly.math.{GenPerm, PhasedInt, Phases}

// TODO rename package to phased

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
  abstract class Op { lhs =>
    def index: Int = indexFromOp(this)
    def unary_- : PhasedOp = lhs * Phase.minusOne
    def *(rhs: Phase): PhasedOp = PhasedOp(rhs, lhs)
    def *(rhs: Op): Mono[Free, Free] = Mono(lhs, rhs)
    def +[A](rhs: A)(implicit ev: ToPoly[A, Free, Free]): Poly[Free, Free] =
      Op.toPoly(lhs) + ev(rhs)
    def -[A](rhs: A)(implicit ev: ToPoly[A, Free, Free]): Poly[Free, Free] =
      Op.toPoly(lhs) - ev(rhs)

    /** Method that, for each operator in this ring, returns its adjoint.
      *
      * For Hermitian operators, the adjoint operation corresponds to the identity.
      */
    def adjoint: Op
  }

  object Op {
    implicit val toPoly: ToPoly[Op, Free, Free] = {  op => symdpoly.Poly.fromMono(Mono.fromOp(op)) }
  }

  abstract class HermitianOp extends Op { selfOp =>
    def adjoint: Op = selfOp
  }

  trait OpType {
    def allInstances: Seq[Op]
  }

  abstract class HermitianType1(iSeq: Seq[Int]) extends OpType {
    val allInstances: Seq[Op] = iSeq.map(apply)
    def apply(i: Int): Op
  }

  abstract class NonHermitianType1(iSeq: Seq[Int]) extends OpType {
    val allInstances: Seq[Op] = for (i <- iSeq; a <- Seq(false, true)) yield apply(i, a)
    def apply(i: Int, adjoint: Boolean): Op
  }

  abstract class Hermitian2Type(iSeq: Seq[Int], jSeq: Int => Seq[Int]) extends OpType {
    def this(iSeq: Seq[Int], jSeq: Seq[Int]) = this(iSeq, i => jSeq)
    val allInstances: Seq[Op] = for (i <- iSeq; j <- jSeq(i)) yield apply(i, j)
    def apply(i: Int, j: Int): Op
  }

  abstract class NonHermitian2Type(iSeq: Seq[Int], jSeq: Int => Seq[Int]) extends OpType {
    def this(iSeq: Seq[Int], jSeq: Seq[Int]) = this(iSeq, i => jSeq)
    val allInstances: Seq[Op] = for (i <- iSeq; j <- jSeq(i); a <- Seq(false, true)) yield apply(i, j, a)
    def apply(i: Int, j: Int, adjoint: Boolean): Op
  }


  case class PhasedOp(phase: Phase, op: Op) { lhs =>
    override def toString: String = Mono[Free](phase, op).toString
    def +[A](rhs: A)(implicit ev: ToPoly[A, Free, Free]): Poly[Free, Free] =
      PhasedOp.toPoly(lhs) + ev(rhs)
    def -[A](rhs: A)(implicit ev: ToPoly[A, Free, Free]): Poly[Free, Free] =
      PhasedOp.toPoly(lhs) - ev(rhs)
  }

  object PhasedOp {
    implicit def fromOp(op: Op): PhasedOp = PhasedOp(Phase.one, op)
    implicit val toPoly: ToPoly[PhasedOp, Free, Free] = {
      case PhasedOp(phase, op) => symdpoly.Poly.single(Mono.fromOp(op), phase.toCyclo)
    }
    implicit val phasedOpGenPermAction: Action[PhasedOp, GenPerm] = new Action[PhasedOp, GenPerm] {
      def actl(g: GenPerm, p: PhasedOp): PhasedOp = actr(p, g.inverse)
      def actr(p: PhasedOp, g: GenPerm): PhasedOp = {
        val PhasedOp(phase, op) = p
        val PhasedInt(newPhase, newIndex) = g.image(PhasedInt(phase, indexFromOp(op)))
        PhasedOp(newPhase, opFromIndex(newIndex))
      }
    }
  }

  def generator(f: Op => PhasedOp)(implicit name: sourcecode.Name): Generator[Free] = {
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

}

object MonoidDef {
  type Aux[F <: MonoidDef with Singleton] = MonoidDef { type Free = F }
}
