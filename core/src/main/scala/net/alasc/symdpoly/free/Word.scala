package net.alasc.symdpoly.free
/*
import java.util.Arrays

import cats.Show
import cats.kernel.Hash
import net.alasc.finite._
import net.alasc.perms.Perm
import shapeless.Witness
import spire.algebra._
import spire.syntax.all._
import net.alasc.algebra.PermutationAction
import net.alasc.attributes.Attributes
import net.alasc.finite.FinitelyGeneratedGrp.Aux
import net.alasc.util.{NNNone, NNOption, NNSome}
import net.alasc.perms.internal.GenPrm.{pairHash, seed}
import net.alasc.symdpoly.algebra.{Morphism}
import org.scalacheck.{Arbitrary, Gen}
import spire.syntax.cfor.cforRange
import spire.util.Opt

import scala.language.dynamics
import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer, WrappedArray}

/** Associative word represented by an array of letter indices.
  *
  * Inverses are represented by ~index . */
class Word[G <: FreeGroup with Singleton](private[symdpoly] val indexArray: Array[Int])(implicit wG: Witness.Aux[G]) { lhs =>
  def G: G = wG.value
  override def equals(any: Any): Boolean = any match {
    case rhs: Word[G] if lhs.G eq rhs.G => G.wordTypeclasses.eqv(lhs, rhs)
  }
  override def toString: String = G.wordTypeclasses.show(lhs)
  private[this] lazy val _hash: Int = G.wordTypeclasses.hash(lhs)
  override def hashCode: Int = _hash
  def length: Int = indexArray.length
  def signedIndex(i: Int): Int = indexArray(i)
  def index(i: Int): Int = if (indexArray(i) < 0) ~indexArray(i) else indexArray(i)
  def isInverse(i: Int): Boolean = indexArray(i) < 0
  def sign(i: Int): Int = if (indexArray(i) < 0) -1 else 1
}

object Word {

  def empty[G <: FreeGroup with Singleton: Witness.Aux]: Word[G] = new Word[G](Array.empty[Int])

  def letter[G <: FreeGroup with Singleton: Witness.Aux](i: Int): Word[G] = new Word[G](Array(i))

  def fromArrayUnsafe[G <: FreeGroup with Singleton: Witness.Aux](indexArray: Array[Int]): Word[G] = new Word[G](indexArray)

  implicit def wordTypeclasses[G <: FreeGroup with Singleton](implicit w: Witness.Aux[G]): Hash[Word[G]] with Group[Word[G]] with Show[Word[G]] =
    (w.value: G).wordTypeclasses

  def genInverseWord[G <: FreeGroup with Singleton: Witness.Aux](rec: Int): Gen[Word[G]] =
    genWord[G](rec - 1).map(_.inverse)

  def genEmpty[G <: FreeGroup with Singleton: Witness.Aux]: Gen[Word[G]] = Gen.const(Word.empty[G])

  def genProduct[G <: FreeGroup with Singleton: Witness.Aux](rec: Int): Gen[Word[G]] = for {
    x <- genWord[G](rec - 1)
    y <- genWord[G](rec - 1)
  } yield x |+| y

  def genLetter[G <: FreeGroup with Singleton](implicit w: Witness.Aux[G]): Gen[Word[G]] = {
    import w.{value => G}
    Gen.choose(0, G.nGenerators - 1).map(Word.letter[G])
  }

  def genWord[G <: FreeGroup with Singleton](rec: Int)(implicit w: Witness.Aux[G]): Gen[Word[G]] =
    if (w.value.nGenerators == 0) genEmpty[G]
    else if (rec > 0) Gen.oneOf(genInverseWord[G](rec), genEmpty[G], genProduct[G](rec), genEmpty[G], genLetter[G])
    else Gen.oneOf(genEmpty[G], genLetter[G])

  implicit def arbWord[G <: FreeGroup with Singleton](implicit w: Witness.Aux[G]): Arbitrary[Word[G]] =
    Arbitrary( genWord[G](3) )

}

/** Definition of a group by its generators along with generator names; enables computations with words. */
class FreeGroup(val generatorNames: Seq[String]) extends FinitelyGeneratedGrp with Dynamic {
  type G = Word[this.type]
  def group: Group[Word[FreeGroup.this.type]] = wordTypeclasses
  def equ: Eq[Word[FreeGroup.this.type]] = wordTypeclasses
  def selectDynamic(fieldName: String): Word[this.type] = generator(generatorNames.indexOf(fieldName))
  override def toString: String = generators.mkString(s"FreeGroup(", ", ", ")")
  implicit val witness: Witness.Aux[this.type] = Witness.mkWitness(this)
  def nGenerators: Int = generatorNames.length
  def generators: Seq[Word[this.type]] = Vector.tabulate(nGenerators)(generator)
  def generator(i: Int): Word[this.type] = Word.letter[this.type](i)
  lazy val wordTypeclasses: WordTypeclasses[this.type] = new WordTypeclasses[this.type]
}

object FreeGroup {
  def apply(generatorNames: String*): FreeGroup = new FreeGroup(generatorNames)
}

final class WordTypeclasses[G <: FreeGroup with Singleton](implicit wG: Witness.Aux[G]) extends Hash[Word[G]] with Group[Word[G]] with Show[Word[G]] {
  def G: G = wG.value

  def show(w: Word[G]): String = {
    val sb = StringBuilder.newBuilder
    @tailrec def writeRun(i: Int, index: Int, cumExp: Int): Int =
      if (i >= w.length || w.index(i) != index) {
        sb ++= G.generatorNames(index)
        if (cumExp != 1) {
          sb ++= "^"
          sb ++= cumExp.toString
        }
        i
      }
      else if (w.signedIndex(i) < 0) writeRun(i + 1, index, cumExp - 1)
      else writeRun(i + 1, index, cumExp + 1)
    @tailrec def writeWord(i: Int, sep: String): Unit =
      if (i < w.length) {
        sb ++= sep
        val newI = writeRun(i + 1, w.index(i), w.sign(i))
        writeWord(newI, " ")
      }
    writeWord(0, "")
    sb.result()
  }

  def eqv(x: Word[G], y: Word[G]): Boolean = Arrays.equals(x.indexArray, y.indexArray)

  def hash(w: Word[G]): Int = Arrays.hashCode(w.indexArray)

  def inverse(a: Word[G]): Word[G] = {
    val newIndices = new Array[Int](a.length)
    cforRange(0 until a.length) { i =>
      newIndices(a.length - i - 1) = ~a.signedIndex(i)
    }
    new Word[G](newIndices)
  }

  def empty: Word[G] = Word.empty[G]

  def combine(x: Word[G], y: Word[G]): Word[G] = {
    @tailrec def iter(xPos: Int, yPos: Int): Array[Int] =
      if (xPos == -1 || yPos >= y.length || x.signedIndex(xPos) != (~y.signedIndex(yPos))) {
        val n = (xPos + 1) + (y.length - yPos)
        val newIndices = new Array[Int](n)
        cforRange(0 to xPos) { i =>
          newIndices(i) = x.signedIndex(i)
        }
        cforRange(0 until (y.length - yPos)) { i =>
          newIndices(xPos + 1 + i) = y.signedIndex(yPos + i)
        }
        newIndices
      }
      else iter(xPos - 1, yPos + 1)
    Word.fromArrayUnsafe[G](iter(x.length - 1, 0))
  }

}

/*
  implicit def forFreeGroupPermutationAction[Source <: FreeGroup with Singleton, T:Eq:Group](implicit sourceW: Witness.Aux[Source]): MorphismFromGeneratorImages[Word[Source], T] =
    new MorphismFromGeneratorImages[Word[Source], T] {
      def apply(source: FinitelyGeneratedGrp.Aux[Word[Source]], images: Seq[T]): Morphism[Word[Source], T, Group] =
        new Morphism[Word[Source], T, Group] {
          def S: Group[Word[Source]] = (sourceW.value: Source).group
          def T: Group[T] = implicitly
          def apply(word: Word[Source]): T = {
            @tailrec def iter(a: T, i: Int): T =
              if (i == word.length) a
              else if (word.sign(i) == 1) iter(a |+| images(word.index(i)), i + 1)
              else iter(a |-| images(word.index(i)), i + 1)
            iter(Group[T].empty, 0)
          }
        }
    }*/

*/