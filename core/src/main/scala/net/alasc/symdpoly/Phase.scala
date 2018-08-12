package net.alasc.symdpoly

import spire.algebra.{Eq, Involution, MultiplicativeAbGroup}

import cyclo.Cyclo

import org.scalacheck.{Arbitrary, Gen}
import org.typelevel.discipline.Predicate

/** Represents a root of unity exp(2*pi*k/n), where k = 0, ..., n - 1.
  *
  * Conditions: n >= 1 and gcd(k, n) == 1
  */
class Phase(val encoding: Int) extends AnyVal { lhs =>
  def isEmpty: Boolean = false
  def _1: Int = k
  def _2: Int = n
  def k: Int = encoding & 0xFFFF
  def n: Int = (encoding >>> 16) & 0xFFFF
  def isOne: Boolean = k == 0
  def isMinusOne: Boolean = (k == 1) && (n == 2)
  def isI: Boolean = (k == 1) && (n == 4)
  def isMinusI = (k == 3) && (n == 4)
  def toInt: Int =
    if (isOne) 1
    else if (isMinusOne) -1
    else throw new IllegalArgumentException(s"Cannot convert phase $this to Int.")
  override def toString: String = (k, n) match {
    case (0, _) => "1"
    case (1, 2) => "-1"
    case (1, 4) => "i"
    case (3, 4) => "-i"
    case _ => {
      val g = spire.math.gcd(2*k, n)
      val num = 2*k/g
      val den = n/g
      if (num == 1) s"exp(pi*i/$den)" else s"exp($num*pi*i/$den)"
    }
  }
  def numeratorIn(den: Int): Int = {
    require(den % n == 0)
    k * (den / n)
  }
  def *(rhs: Phase): Phase =
  if (lhs.isOne) rhs
  else if (rhs.isOne) lhs
  else {
    val ln = lhs.n
    val rn = rhs.n
    val lk = lhs.k
    val rk = rhs.k
    val lcm: Long = spire.math.lcm(ln, rn)
    val num: Long = (lk * (lcm/ln) + rk * (lcm/rn)) % lcm
    val gcd: Long = spire.math.gcd(num, lcm)
    val k = num / gcd
    val n = lcm / gcd
    assert(k.isValidInt && n.isValidInt)
    Phase(k.toInt, n.toInt)
  }
  def reciprocal: Phase = adjoint
  def /(rhs: Phase): Phase = lhs*(rhs.reciprocal)
  def adjoint: Phase = Phase((n - k) % n, n)
  def unary_- : Phase = lhs * Phase.minusOne
  def compareTo(rhs: Phase): Int =
    if (lhs.encoding == rhs.encoding) 0
    else {
      val lcm = spire.math.lcm(lhs.n, rhs.n)
      java.lang.Long.compare(lhs.k * (lcm / lhs.n), rhs.k * (lcm / rhs.n)).signum
    }
  def toCyclo: Cyclo = Cyclo.e(n).pow(k)
}

final class PhaseInstances extends Eq[Phase] with MultiplicativeAbGroup[Phase] with Involution[Phase] {
  def div(x: Phase, y: Phase): Phase = x / y
  override def reciprocal(x: Phase): Phase = x.reciprocal
  def one: Phase = Phase(0, 1)
  def times(x: Phase, y: Phase): Phase = x * y
  def adjoint(x: Phase): Phase = x.adjoint
  def eqv(x: Phase, y: Phase): Boolean = x.encoding == y.encoding
}

object Phase {
  // implicit conversion
  implicit def toCyclo(p: Phase): Cyclo = p.toCyclo

  def unapply(e: Phase): Phase = e
  val one = Phase(0, 1)
  val i = Phase(1, 4)
  val minusOne = Phase(1, 2)
  val minusI = Phase(3, 4)
  def apply(n: Int): Phase = Phase(1, n)
  def encode(k: Int, n: Int): Int = {
    require(n >= 1 && n <= 0xFFFF)
    require(k >= 0 && k < n)
    require(spire.math.gcd(k, n) == 1)
    (n << 16) + k
  }
  def fromEncoding(encoding: Int): Phase = new Phase(encoding)

  def apply(k: Int, n: Int): Phase = {
    import spire.std.int._
    import spire.syntax.truncatedDivision._
    require(n >= 1)
    val ek = if (k >= 0 && k < n) k else (k fmod n)
    val g = spire.math.gcd(ek, n).toInt
    new Phase(encode(ek / g, n / g))
  }

  private[this] val instance = new PhaseInstances
  implicit def equ: Eq[Phase] = instance
  implicit def multiplicativeAbGroup: MultiplicativeAbGroup[Phase] = instance
  implicit def involution: Involution[Phase] = instance

  // Generator for a random free monomial
  def gen: Gen[Phase] = for {
    n <- Gen.choose(1, 6)
    k <- Gen.choose(0, n - 1)
  } yield Phase(k, n)

  // for tests
  implicit def nonZero: Predicate[Phase] = Predicate(x => true)
  implicit val arb: Arbitrary[Phase] = Arbitrary(gen)

}
