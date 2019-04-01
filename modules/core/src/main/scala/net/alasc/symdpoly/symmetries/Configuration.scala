package net.alasc.symdpoly
package symmetries

import scala.annotation.tailrec

import shapeless.Witness

import scalin.immutable.{DenseMat, Mat, Vec}
import scalin.immutable.dense._

import net.alasc.finite.Grp
import net.alasc.partitions.Partition
import cyclo.Cyclo
import spire.syntax.cfor._

import net.alasc.symdpoly.math.{GenPerm, GrpMonomialRepresentation, Phase, PhasedInt}
import metal.syntax._

/** Configuration matrix that encodes a matrix invariant under a monomial group representation.
  *
  * Maximal size is 2**16 = 65536, such that (row, col) cell indices can be encoded in a Int
  * (see Ptr singleton methods to encode/decode indices).
  *
  * Orbits of this configuration are stored using a linked list. The start of each orbit
  * is given by _orbitStart, and the next element by _orbitNext.
  *
  * _orbitNext, _phase, _orbit are actually a n x n matrix of integers, stored using
  * column major order.
  *
  * @param n            Size of the square matrix (e.g. number of rows)
  * @param _orbitStart  Cell index of the orbit start, array of size "nOrbits", the index
  *                     is encoded as an integer according to the Ptr convention.
  * @param _orbitNext   Cell index of the next element in the orbit
  *                     The special value "0" encodes the end of the chain -- as the cell index 0
  *                     or (0, 0), when it is part of an orbit, is always the first element of that
  *                     orbit.
  *                     When nonzero, the index is encoded according to the Ptr convention.
  * @param _phase       Phase of an element with respect to the first element in its orbit.
  * @param _orbit       Orbit index of an element.
  */
final class Configuration(val n: Int,
                          _orbitStart: Array[Int],
                          _orbitNext: Array[Int],
                          _orbit: Array[Int],
                          _phase: Array[Int]) {

  override def toString: String =
    scalin.Printer.mat(
      Mat.tabulate(n, n) { (r, c) =>
      orbit(r, c) match {
        case -1 => "0"
        case o => phase(r, c) match {
          case Phase.one => s"x$o"
          case Phase.minusOne => s"-x$o"
          case phase => s"$phase x$o"
        }
      }
    })

  require(n <= 65536)

  private[this] def index(r: Int, c: Int): Int = {
    require(r >= 0 && r < n, s"Invalid row index $r")
    require(c >= 0 && c < n, s"Invalid column index $c")
    r + c * n
  }

  def nOrbits: Int = _orbitStart.length
  def orbitStart(o: Int): Ptr[this.type] = new Ptr[this.type](_orbitStart(o))

  def ptr(r: Int, c: Int): Ptr[this.type] =
    if (orbit(r, c) == -1) Ptr.empty[this.type] else Ptr.nonEmpty[this.type](r, c)

  private[this] def ptrIndex(ptr: Ptr[this.type]): Int = {
    require(!ptr.isEmpty, "Pointer cannot be empty for this operation")
    ptr.row + ptr.col * n
  }

  def nextPtr(ptr: Ptr[this.type]): Ptr[this.type] = _orbitNext(ptrIndex(ptr)) match {
    case 0 => Ptr.empty
    case i => Ptr.nonEmpty(i)
  }

  /** Returns the orbit in which the cell (r,c) is contained or -1 if the cell is zero. */
  def orbit(r: Int, c: Int): Int = _orbit(index(r, c))

  /** Returns the phase of the cell (r,c) with respect to the element at the start of its orbit. */
  def phase(r: Int, c: Int): Phase = new Phase(_phase(index(r, c)))

  /** Given a vector of values, such that values(o) gives the value of the element
    * at orbitStart(o), returns the corresponding matrix. */
  def toMatrix(values: Vec[Cyclo]): Mat[Cyclo] = Mat.tabulate(n, n) { (r, c) =>
    orbit(r, c) match {
      case -1 => Cyclo.zero
      case o => values(o) * phase(r, c).toCyclo
    }
  }

  /** Given a matrix of values, return the vector of values, if it exists,
    * such that toMatrix(values) returns the matrix. */
  def fromMatrix(mat: Mat[Cyclo]): Option[Vec[Cyclo]] = {
    val values = new Array[Cyclo](nOrbits)
    cforRange(0 until nOrbits) { o =>
      val startPtr = orbitStart(o)
      val startValue = mat(startPtr.row, startPtr.col)
      @tailrec def rec(ptr: Ptr[this.type]): Boolean =
        if (ptr.isEmpty) true else {
          val r = ptr.row
          val c = ptr.col
          if (mat(r, c) === startValue * ptr.phase)
            rec(ptr.next)
          else
            false
        }
      if (!rec(startPtr.next))
        return None
      else
        values(o) = startValue
    }
    Some(Vec.fromSeq(values))
  }

}

/** Pointer in a configuration that either represents:
  *
  * - Some (r, c) pair when "i" fits in an unsigned 32 bits integer. In that case,
  *   the 16 most significant bits encode the row, and the 16 least significant bits
  *   encode the column.
  * - The empty value when i is a negative Long (normally -1).
  */
class Ptr[C <: Configuration with Singleton](val i: Long) extends AnyVal {
  def row(implicit C: Witness.Aux[C]): Int = i.toInt >>> 16
  def col(implicit C: Witness.Aux[C]): Int = i.toInt & 0xFFFF
  def phase(implicit C: Witness.Aux[C]): Phase = valueOf[C].phase(row, col)
  def isEmpty: Boolean = i < 0
  def next(implicit C: Witness.Aux[C]): Ptr[C] = valueOf[C].nextPtr(this)
}

object Ptr {
  def nonEmpty[C <: Configuration with Singleton](i: Int): Ptr[C] = new Ptr[C](i)
  def nonEmpty[C <: Configuration with Singleton](r: Int, c: Int): Ptr[C] = new Ptr[C]((r << 16) + c)
  def empty[C <: Configuration with Singleton]: Ptr[C] = new Ptr[C](-1L)
  // for reference
  def encode(r: Int, c: Int): Int = (r << 16) + c
  def row(i: Int): Int = i >>> 16
  def col(i: Int): Int = i & 0xFFFF
}

object Configuration {

  /** Returns the configuration corresponding to matrices that are invariant under the given monomial representation. */
  def fromGrpMonomialRepresentation[G](gm: GrpMonomialRepresentation[G]): Configuration =
    Configuration(gm.n, gm.generatorImages)

  /** Returns a trivial configuration, i.e. one where all matrix cells are independent. */
  def trivial(n: Int): Configuration = {
    val orbitStart = new Array[Int](n * n)
    val orbitNext = new Array[Int](n * n)
    val orbit = new Array[Int](n * n)
    val phase = Array.fill(n * n)(Phase.one.encoding)
    @tailrec def rec(ind: Int, r: Int, c: Int): Unit =
      if (c == n) ()
      else if (r == n) rec(ind, 0, c + 1)
      else {
        orbitStart(ind) = (r << 16) + c
        orbit(ind) = ind
        rec(ind + 1, r + 1, c)
      }
    rec(0, 0, 0)
    new Configuration(n, orbitStart, orbitNext, orbit, phase)
  }

  def apply(n: Int, grp: Grp[GenPerm]): Configuration = apply(n, grp.generators)

  /** Use an orbit enumeration algorithm to generate a configuration from a group of generalized permutations.
    *
    * We use the following convention. Let X(r,c) be a cell of the configuration matrix, and g a generalized
    * permutation. We must have X(r1, c1) = X(r,c) * pr.reciprocal * pc, where
    * (pr, r1) is the image of (Phase.one, r) under g, and
    * (pc, c1) is the image of (Phase.one, c) under g.
    *
    */
  def apply(n: Int, generators: Seq[GenPerm]): Configuration =
  if (generators.isEmpty) trivial(n) else {
    /** Index in the column major storage of the matrix. */
    def index(r: Int, c: Int): Int = r + c * n
    /** Encoding of both (r, c) as a single Int. */
    def encode(r: Int, c: Int): Int = (r << 16) + c
    /** Row in the encoded Int. */
    def row(encoding: Int): Int = encoding >>> 16
    /** Column in the encoded Int. */
    def col(encoding: Int): Int = encoding & 0xFFFF
    /** Growable array of integers representing the cells that start an orbit. */
    val orbitStart = metal.mutable.Buffer.empty[Int]
    /** Column major storage of the encoding of next elements. */
    val orbitNext = new Array[Int](n * n)
    /** Column major storage of the orbit number, -2 when not filled up yet, -1 when the orbit has been set to zero */
    val orbit = Array.fill(n * n)(-2)
    /** Column major storage of phase encodings. */
    val phase = new Array[Int](n * n)
    /** Stack of row and column */
    val stack = metal.mutable.Buffer.empty[Int]
    /** Put group generators in array to guarantee faster access. */
    val generatorArray = generators.toArray
    /** Iterate through the matrix, working on the cells that have not been identified as
      * part of an orbit.
      *
      * We iterate in column major order.
      */
    @tailrec def identifyNextOrbit(nOrbits: Int, r: Int, c: Int): Int =
      if (c == n) nOrbits // end
      else if (r == n) identifyNextOrbit(nOrbits, 0, c + 1) // next column
      else if (orbit(index(r, c)) != -2) identifyNextOrbit(nOrbits, r + 1, c) // has been done already, next
      else {
        // we start on (r, c), encode it and put it in the stack of elements to iterate under
        // the group generators
        orbit(index(r, c)) = nOrbits
        phase(index(r, c)) = Phase.one.encoding
        stack += encode(r, c)

        // iterate while the stack is not empty to identify all elements of the orbit
        @tailrec def iterateOrbit(isZero1: Boolean, prevR: Int, prevC: Int): Boolean =
          if (stack.isEmpty) isZero1 else {
            // pop the last element of the stack
            val encoding1 = stack(stack.length - 1)
            stack.length -= 1
            val r1 = row(encoding1)
            val c1 = col(encoding1)
            val phase1 = new Phase(phase(index(r1, c1)))
            // we add elements to the orbit linked list when they are removed from the stack,
            // with the exception of the first orbit element (which has no previous element)
            if (prevR != -1 && prevC != -1)
              orbitNext(index(prevR, prevC)) = encode(r1, c1)

            // iterate the action of all generators on the current cell (r1, c1)
            @tailrec def iterateGenerators(isZero2: Boolean, i: Int): Boolean =
              if (i == generatorArray.length) isZero2 else {
                // compute the action of the generator on (r1, c1)
                val g = generatorArray(i)
                val PhasedInt(pr, r2) = g.image(PhasedInt(Phase.one, r1))
                val PhasedInt(pc, c2) = g.image(PhasedInt(Phase.one, c1))
                // and compute the phase of the new element, relative to the first orbit element
                // thus we need to multiply the phase difference of the generator action to the
                // phase of the element acted upon
                val phase2 = phase1 * pr.reciprocal * pc
                orbit(index(r2, c2)) match { // we check whether the new element in the orbit...
                  case -2 => // has not been identified, so we add it to the orbit
                    stack += encode(r2, c2) // stack it to examine it later
                    orbit(index(r2, c2)) = nOrbits
                    phase(index(r2, c2)) = phase2.encoding
                    iterateGenerators(isZero2, i + 1)
                  case -1 => // element already identified as part of the zero orbit, so we are zero
                    iterateGenerators(true, i + 1)
                  case o if o == nOrbits => // in the current orbit
                    val phaseMismatch = (phase(index(r2, c2)) != phase2.encoding) // check for phase mismatch
                    iterateGenerators(isZero2 || phaseMismatch, i + 1)
                  case _ => sys.error("Iterating over another orbit should have produced every point of it already")
                }
              }
            iterateOrbit(iterateGenerators(isZero1, 0), r1, c1)
          }

        // identify the current orbit, and return whether it is zero
        val isZero = iterateOrbit(false, -1, -1)

        // set an orbit to zero, where (r1, c1) is the orbit start
        // this function iterates through the linked list
        @tailrec def setOrbitToZero(r1: Int, c1: Int): Unit = {
          orbit(index(r1, c1)) = -1
          phase(index(r1, c1)) = Phase.one.encoding
          val encoding = orbitNext(index(r1, c1))
          if (encoding != 0)
            setOrbitToZero(row(encoding), col(encoding))
        }

        if (isZero) { // if zero, set the orbit to zero and iterate
          setOrbitToZero(r, c)
          identifyNextOrbit(nOrbits, r + 1, c)
        } else { // if nonzero, add the orbit start, increment the orbit number and iterate
          orbitStart += encode(r, c)
          identifyNextOrbit(nOrbits + 1, r + 1, c)
        }
      }
    // launch the main loop
    identifyNextOrbit(0, 0, 0)
    new Configuration(n, orbitStart.toArray, orbitNext, orbit, phase)
  }

}
