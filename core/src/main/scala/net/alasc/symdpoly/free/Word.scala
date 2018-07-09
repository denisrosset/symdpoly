package net.alasc.symdpoly.free

import org.scalacheck.Gen
import shapeless.Witness
import spire.algebra.{MultiplicativeMonoid, Order}

import net.alasc.symdpoly.{Mono, Phase}

/** A nonzero word with phase one. */
case class Word[F <: MonoidDef.Aux[F] with Singleton](data: MutableWord[F]) { lhs =>
  def isOne: Boolean = data.isOne
  require(data.phase.isOne)
  require(!data.isZero)
  require(!data.mutable)
  implicit def wF: Witness.Aux[F] = data.wF
  def M: F = wF.value
  override def toString: String = data.toString
  def toMutableGradedWord: MutableWord[F] = data.mutableCopy
  def toMono: Mono[F, F] = new Mono[F, F](data)
}

object Word {

  // Generator for a random free monomial
  def gen[M <: MonoidDef.Aux[M] with Singleton](implicit wM: Witness.Aux[M]): Gen[Word[M]] = {
    import wM.{value => M}
    for {
      length <- Gen.choose(0, 8)
      indices <- Gen.containerOfN[Array, Int](length, Gen.choose(0, M.nOperators - 1))
    } yield Word(new MutableWord[M](Phase.one, length, indices, false))
  }

  def one[M <: MonoidDef.Aux[M] with Singleton:Witness.Aux]: Word[M] = new Word(MutableWord.one[M].setImmutable())

}

final class WordTypeclasses[M <: MonoidDef.Aux[M] with Singleton](implicit val wM: Witness.Aux[M])
  extends MultiplicativeMonoid[Word[M]] with Order[Word[M]] {
  def M: M = wM.value
  def one: Word[M] = Word.one[M]
  def compare(x: Word[M], y: Word[M]): Int = x.data.compareToIgnoringPhase(y.data)
  def times(x: Word[M], y: Word[M]): Word[M] =
    if (x.isOne) y
    else if (y.isOne) x
    else {
      val res = x.data.mutableCopy(x.data.length + y.data.length)
      res *= y.data
      Word(res.setImmutable())
    }

}
