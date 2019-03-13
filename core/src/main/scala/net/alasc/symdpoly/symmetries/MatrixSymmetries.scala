package net.alasc.symdpoly
package symmetries

import cats.Contravariant
import shapeless.Witness
import spire.algebra.{Action, Group, Monoid, Order}
import spire.std.unit._
import spire.syntax.action._
import spire.syntax.cfor._

import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.{GrpChain, GrpChainPermutationAction}
import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.perms.Perm
import net.alasc.perms.default._
import net.alasc.symdpoly.algebra.{InjectiveMorphism, Morphism, Phased}
import net.alasc.symdpoly.generic.SymmetryEquivalence
import net.alasc.symdpoly.math.{GenPerm, Phase, Phases}
import net.alasc.symdpoly.util.OrderedSet
import instances.invariant._
import syntax.all._
import net.alasc.std.product._
import spire.std.tuples._

/** Describes the symmetries of a matrix.
  *
  * @param grp Group of symmetries
  * @param representation Group representation,
  * @tparam G Generic group element type
  *
  */
case class MatrixSymmetries[G](n: Int, grp: Grp[G], representation: Morphism[G, GenPerm, Group]) {

  def generatorImages: Seq[GenPerm] = grp.generators.map(representation.apply)

  /** Nice injective morphism from G to permutations, used to study the group. */
  lazy val niceMorphism: InjectiveMorphism[G, Perm, Group] = {
    val action: PermutationAction[G] = grp match {
      case grpChain: GrpChain[G, a] if grpChain.action.isFaithful => grpChain.action
      case _ => Contravariant[PermutationAction].contramap(GenPerm.fpab.apply(generatorImages))(representation.apply)
    }
    InjectiveMorphism[G, Perm, Group](action.toPerm)(representation.S, implicitly)
  }

  /** Returns */
  lazy val onPermutationGroup: MatrixSymmetries[Perm] = {
    val permGenerators = grp.generators.map(niceMorphism)
    val grp1 = Grp.fromGeneratorsAndOrder(permGenerators, grp.order)
    val representation1 = Morphism.fromGeneratorImages(grp1, generatorImages)
    MatrixSymmetries[Perm](n, grp1, representation1)
  }

  /** Returns the configuration corresponding to matrices that are invariant under this group monomial representation. */
  lazy val configuration: Configuration = Configuration(n, generatorImages)

}

object MatrixSymmetries {

  /** Computes the generalized permutation corresponding to the action of an element of type "G" on the domain "set".
    *
    * @param set Ordered set of domain elements, containing only canonical representatives
    * @param g   Group element
    */
  def genPerm[A:Order:Phased, G](set: OrderedSet[A], g: G)(implicit action: Action[A, G]): GenPerm = {
    import scala.collection.mutable.{HashMap => MMap}
    val phaseMap: MMap[Int, Phase] = MMap.empty[Int, Phase]
    val n = set.length
    val permImages = new Array[Int](n)
    cforRange(0 until n) { i =>
      val image = set(i) <|+| g
      val canonical = image.phaseCanonical
      val phase = image.phaseOffset
      val permImage = set.indexOf(canonical)
      permImages(i) = permImage
      phaseMap(permImage) = phase
    }
    val perm = Perm.fromImages(permImages)
    val phases = Phases(phaseMap.toVector: _*)
    GenPerm(perm, phases)
  }

  /** Extract the matrix symmetries from equivalence relations of an evaluator. */
  def fromEquivalences[
    M <: generic.MonoidDef with Singleton:Witness.Aux
  ](seq: Seq[generic.Equivalence[M]], generatingSet: OrderedSet[M#Monomial]): MatrixSymmetries[_] =
      monoid(generatingSet.length).combineAll(seq.collect {
        case e: generic.SymmetryEquivalence[M, typeG] => fromEquivalence(e, generatingSet)
      })

  /** Extract the matrix symmetries from a symmetry equivalence relation. */
  def fromEquivalence[
    M <: generic.MonoidDef with Singleton:Witness.Aux,
    G
  ](e: generic.SymmetryEquivalence[M, G], generatingSet: OrderedSet[M#Monomial]): MatrixSymmetries[G] = {
    val grp: Grp[G] = e.grp
    import e.action
    import grp.{equ, group}
    val representation: Morphism[G, GenPerm, Group] = Morphism[G, GenPerm, Group](g => genPerm(generatingSet, g))
    MatrixSymmetries(generatingSet.length, grp, representation)
  }

  def monoid(n: Int): Monoid[MatrixSymmetries[_]] = new Monoid[MatrixSymmetries[_]] {
    def empty: MatrixSymmetries[_] = MatrixSymmetries(n, Grp.trivial[Unit], Morphism[Unit, GenPerm, Group](x => GenPerm.id))
    def combine(x: MatrixSymmetries[_], y: MatrixSymmetries[_]): MatrixSymmetries[_] =
      if (x.grp.isTrivial) y
      else if (y.grp.isTrivial) x
      else {
        val s1 = x.onPermutationGroup
        val s2 = y.onPermutationGroup
        val generators = s1.grp.generators.map(p => (p, Perm.id)) ++ s2.grp.generators.map(p => (Perm.id, p))
        val images = s1.generatorImages ++ s2.generatorImages
        val grp = Grp.fromGeneratorsAndOrder(generators, s1.grp.order * s2.grp.order)
        val representation = Morphism.fromGeneratorImages(grp, images)
        MatrixSymmetries(n, grp, representation)
      }
  }

}
