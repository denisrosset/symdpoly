package net.alasc.symdpoly.free

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

case class PermutationActionByImages[G <: GroupWithGenerators[A] with Singleton, A](images: Seq[Perm], invImages: Seq[Perm], isFaithful: Boolean = false)
  extends PermutationAction[Word[G, A]] {
  import Word.{indexMask, signMask}
  val permSize = images.map(_.largestMovedPoint.getOrElseFast(-1)).reduceOption(spire.math.max(_, _)).getOrElse(-1) + 1
  def permForSignedIndex(signedIndex: Int): Perm =
    if (signedIndex >= 0) images(signedIndex) else invImages(signedIndex & indexMask)

  def findMovedPoint(g: Word[G, A]): NNOption = {
    cforRange(0 until permSize) { i =>
      if (actr(i, g) != i) return NNSome(i)
    }
    NNNone
  }

  def movedPointsUpperBound(g: Word[G, A]): NNOption = NNSome(permSize - 1)

  def actr(p: Int, w: Word[G, A]): Int = {
    @tailrec def iter(c: Int, i: Int): Int =
      if (i == w.length) c
      else iter(permForSignedIndex(w.signedIndex(i)).image(c), i + 1)
    iter(p, 0)
  }

  def actl(w: Word[G, A], p: Int): Int = {
    @tailrec def iter(c: Int, i: Int): Int =
      if (i == -1) c
      else iter(permForSignedIndex(w.signedIndex(i) ^ signMask).image(c), i - 1)
    iter(p, w.length - 1)
  }
}

/** Describes a simple program that computes the orbit of an element under the action of a group. Each instruction is of the form
  * dest := src <|+| gen, where gen is a generator of the group. The source, destination and generator indices are stored in
  * flat arrays.
  */
class ActionProgram[G <: GroupWithGenerators[A] with Singleton, A](val groupSize: Int, val src: Array[Int], val dest: Array[Int], val gen: Array[Int])(implicit w: Witness.Aux[G]) {
  def G: G = w.value
  override def toString: String =
    "x0 = mono\n" + Seq.tabulate(src.length)(i => s"x${dest(i)} := x${src(i)} <|+| ${G.generatorName(gen(i))}").mkString("\n")
  def programLength: Int = src.length
}

object ActionProgram {

  import scala.collection.mutable.{LongMap, HashSet => MutableHashSet, HashMap => MutableHashMap, Queue => MutableQueue}

  case class CayleyGraph[G <: GroupWithGenerators[A] with Singleton, A]()(implicit val wG: Witness.Aux[G]) {
    def G: G = wG.value
    class Node(val word: Word[G, A]) {
      private[this] val knownChildren: LongMap[Node] = LongMap.empty
      // equals is by reference (nodes are unique)
      // hashCode is by reference (nodes are unique)
      def apply(i: Int): Node = {
        def newChild: Node = {
          val newWord = word |+| G.generatorWords(i)
          nodes.getOrElseUpdate(newWord, new Node(newWord))
        }
        knownChildren.getOrElseUpdate(i, newChild)
      }
      def children: Seq[Node] = Seq.tabulate(G.nGenerators)(i => apply(i))
    }
    val root: Node = new Node(Word.empty[G, A])
    val nodes: MutableHashMap[Word[G, A], Node] = MutableHashMap(Word.empty[G, A] -> root)
    def path(word: Word[G, A]): Seq[Node] = (0 until word.length).scanLeft(root) {
      case (node, i) => node(word.index(i))
    }
  }

  object Attributes extends Attributes("Grp[Word]") {
    object SimpleEnumeration extends Attribute("SimpleEnumeration") {
      implicit def forGroupWithGenerators[G <: GroupWithGenerators[A] with Singleton, A]: For[Grp[Word[G, A]], Seq[Word[G, A]]] = For
    }
  }

  private[this] def computeSimpleEnumeration[G <: GroupWithGenerators[A] with Singleton, A](subgroup: Grp[Word[G, A]])(implicit w: Witness.Aux[G]): Seq[Word[G, A]] = {
    val graph = CayleyGraph[G, A]()
    val included = MutableHashSet(graph.root)
    val obtained = MutableHashSet(graph.root)
    val remaining = subgroup.iterator.filterNot(_.isEmpty).to[MutableHashSet]
    val total = subgroup.order.toInt
    val progress = new ProgressBar("Action program", total, 200)
    progress.step()
    while (remaining.nonEmpty) {
      val queue = obtained.to[MutableQueue]
      val visited = obtained.clone
      while (queue.nonEmpty) {
        val node = queue.dequeue()
        val children: Seq[graph.Node] = node.children.filterNot(visited.contains)
        visited ++= children
        queue.enqueue(children: _*)
        children.find(c => remaining.contains(c.word)) match {
          case Some(newNode: graph.Node) =>
            included += newNode
            remaining -= newNode.word
            obtained ++= graph.path(newNode.word)
            queue.clear()
          case None => // do nothing
        }
      }
      progress.stepTo(total - remaining.size)
    }
    progress.close()
    included.map(_.word).toVector
  }
  def findSimpleEnumeration[G <: GroupWithGenerators[A] with Singleton, A](subgroup: Grp[Word[G, A]])(implicit w: Witness.Aux[G]): Seq[Word[G, A]] =
    Attributes.SimpleEnumeration(subgroup) { computeSimpleEnumeration(subgroup) }

  class Node[G <: GroupWithGenerators[A] with Singleton, A](val word: Word[G, A], val parent: Opt[Node[G, A]], val children: LongMap[Node[G, A]], var included: Boolean = false) {
    override def equals(any: Any): Boolean = any match {
      case that: Node[G, A] => word == that.word
      case _ => false
    }
    override def hashCode: Int = word.hashCode
    def apply(i: Int)(implicit w: Witness.Aux[G]): Node[G, A] = {
      def G: G = w.value
      def newNode: Node[G, A] = {
        val newWord = word |+| G.generatorWords(i)
        new Node[G, A](newWord, Opt(this), LongMap.empty[Node[G, A]])
      }
      children.getOrElseUpdate(i, newNode)
    }
    def nodeAndParents: List[Node[G, A]] = parent match {
      case Opt(parent) => this :: parent.nodeAndParents
      case _ => this :: Nil
    }
  }

  def findSimpleEnumeration1[G <: GroupWithGenerators[A] with Singleton, A](subgroup: Grp[Word[G, A]])(implicit w: Witness.Aux[G]): Seq[Word[G, A]] = {
    def G: G = w.value
    val root = new Node(Word.empty[G, A], Opt.empty[Node[G, A]], LongMap.empty[Node[G, A]], true)
    val obtained = MutableHashSet(root)
    val remaining = subgroup.iterator.filterNot(_.isEmpty).to[MutableHashSet]
    val total = subgroup.order.toInt
    val progress = new ProgressBar("Action program", total, 200)
    progress.step()
    while (remaining.nonEmpty) {
      val queue = obtained.to[MutableQueue]
      val reached = obtained.clone
      while (queue.nonEmpty) {
        val node = queue.dequeue()
        val children = Seq.tabulate(G.nGenerators)(i => node(i)).filterNot(reached.contains)
        reached ++= children
        queue.enqueue(children: _*)
        children.find(c => remaining.contains(c.word)) match {
          case Some(newNode) =>
            remaining -= newNode.word
            obtained ++= newNode.nodeAndParents
            newNode.included = true
            queue.clear()
          case None => // do nothing
        }
      }
      progress.stepTo(total - remaining.size)
    }
    progress.close()
    obtained.filter(_.included).map(_.word).toVector
  }

  def apply[G <: GroupWithGenerators[A] with Singleton, A](subgroup: Grp[Word[G, A]])(implicit w: Witness.Aux[G]): ActionProgram[G, A] =
    constructActionProgram(findSimpleEnumeration[G, A](subgroup))

  case class Tree[G <: GroupWithGenerators[A] with Singleton, A](included: Boolean, branches: Map[Int, Tree[G, A]])

  def constructActionProgram[G <: GroupWithGenerators[A] with Singleton, A](elements: Seq[Word[G, A]])(implicit w: Witness.Aux[G]): ActionProgram[G, A] = {
    def constructTree(seq: Seq[Word[G, A]]): Tree[G, A] = {
      val groups = seq.groupBy(w => if (w.length == 0) -1 else w.index(0))
      val included = groups.isDefinedAt(-1)
      val branches = groups.filterKeys(_ != -1).mapValues(seq => constructTree(seq.map(_.drop(1))))
      Tree[G, A](included, branches)
    }
    val tree = constructTree(elements)
    var lastIndex = 0
    val srcB = ArrayBuffer.empty[Int]
    val destB = ArrayBuffer.empty[Int]
    val genB = ArrayBuffer.empty[Int]
    def instruction(src: Int, dest: Int, gen: Int): Unit = {
      srcB += src
      destB += dest
      genB += gen
    }
    def iterate(node: Tree[G, A], index: Int): Unit =
      if (node.branches.size == 0) {
        require(node.included)
        // do nothing, keep the element
      } else {
        val bseq = node.branches.toSeq
        val limit = if (node.included) bseq.length else bseq.length - 1
        cforRange(0 until limit) { i =>
          val (g, subNode) = bseq(i)
          val newIndex = lastIndex + 1
          lastIndex = newIndex
          instruction(index, newIndex, g)
          iterate(subNode, newIndex)
        }
        if (!node.included) {
          // reuse the current index
          val (g, subNode) = bseq.last
          instruction(index, index, g)
          iterate(subNode, index)
        }
      }
    iterate(tree, 0)
    println(s"Created action straight line program with ${srcB.length} steps for ${elements.length} elements")
    new ActionProgram[G, A](lastIndex + 1, srcB.toArray, destB.toArray, genB.toArray)
  }

}
*/