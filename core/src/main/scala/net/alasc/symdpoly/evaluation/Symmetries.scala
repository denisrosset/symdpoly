package net.alasc.symdpoly
package evaluation

import scala.annotation.tailrec

import shapeless.Witness
import spire.syntax.action._
import spire.syntax.cfor._

import cyclo.Cyclo
import metal.syntax._
import metal.{IsVPtr, Ptr}

import net.alasc.finite.Grp
import net.alasc.partitions.Partition
import net.alasc.perms.Perm
import net.alasc.perms.default._
import net.alasc.symdpoly.math.{GenPerm, GenPermFaithfulPermutationAction, Phases}
import net.alasc.symdpoly.{Mono, OrderedSet, free, generic}

object Symmetries {

  /** Returns the action of the given generalized permutation (on operators) on the given
    * ordered set of monomials. */
  def momentSetAction[
    E <: FreeBasedEvaluator[M, F] with Singleton:Witness.Aux,
    M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](monomials: OrderedSet[EvaluatedMono[E, M, M#TrivialGroup]], g: GenPerm): GenPerm = {
    import scala.collection.mutable.{HashMap => MMap}
    implicit val actImp = EvaluatedMono.genPermAction[E, M, F, M#TrivialGroup]
    val phaseMap: MMap[Int, Phase] = MMap.empty[Int, Phase]
    val n = monomials.length
    val permImages = new Array[Int](n)
    cforRange(0 until n) { i =>
      val image = monomials(i) <|+| g
      val canonical = new EvaluatedMono[E, M, M#TrivialGroup](image.normalForm.phaseCanonical)
      val phase = image.normalForm.phaseOffset
      val permImage = monomials.indexOf(canonical)
      permImages(i) = permImage
      phaseMap(permImage) = phase
    }
    val perm = Perm.fromImages(permImages)
    val phases = Phases(phaseMap.toVector: _*)
    GenPerm(perm, phases)
  }

  /** Returns all evaluated monomials present in the orbit of the given polynomial under
    * the given symmetry group. */
  def allEvaluatedMonomials[
    E <: FreeBasedEvaluator[M, F] with Singleton:Witness.Aux,
    M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux
  ](poly: EvaluatedPoly[E, M, M#TrivialGroup], grp: Grp[GenPerm]): OrderedSet[EvaluatedMono[E, M, M#TrivialGroup]] = {
    val orbit = metal.mutable.HashSet.empty[free.MutableWord[F]]
    val remaining = metal.mutable.HashSet.empty[free.MutableWord[F]]
    cforRange(0 until poly.normalForm.nTerms) { i =>
      remaining += poly.normalForm.monomialNormalForm(i)
    }
    val mono = free.MutableWord.one[F]
    val pad = valueOf[E].makeScratchPad
    val trivialGroupArray = Array(GenPerm.id)
    @tailrec def iter(): Unit = remaining.ptr match {
      case IsVPtr(vp) =>
        val m = vp.key
        mono.setToContentOf(m)
        grp.iterator.foreach { g =>
          mono.setToContentOf(m)
          mono.applyGenPermAction(g)
          valueOf[E].reduceInScratchPad(mono, trivialGroupArray, pad = pad)
          assert(!mono.isZero) // cannot be zero as it was part of a polynomial normal form
          mono.setPhase(Phase.one)
          remaining.ptrFind(mono) match {
            case IsVPtr(vp1) =>
              vp1.remove
            case _ =>
          }
          if (!orbit.contains(mono))
            orbit += mono.immutableCopy
        }
        iter()
      case _ =>
    }
    iter()
    val array = new Array[free.MutableWord[F]](orbit.size)
    def addToArray(ptr: Ptr[orbit.type], i: Int): Unit = ptr match {
      case IsVPtr(vp) =>
        array(i) = vp.key
        addToArray(vp.next, i + 1)
      case _ =>
    }
    addToArray(orbit.ptr, 0)
    spire.math.Sorting.quickSort(array)
    new OrderedSet(array.map(word => (new EvaluatedMono[E, M, M#TrivialGroup](new Mono[M, F](word))).asInstanceOf[AnyRef]))
  }

  /** Computes the symmetry group of the given polynomial under the given ambient group. */
  def symmetrySubgroup[
    E <: FreeBasedEvaluator[M, F] with Singleton:Witness.Aux,
    M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux
  ](poly1: EvaluatedPoly[E, M with generic.FreeBasedMonoidDef.Aux[F], M#TrivialGroup], ambientGroup: Grp[GenPerm]): Grp[GenPerm] = {
    val poly: EvaluatedPoly[E, M, M#TrivialGroup] = poly1
    val monomials = allEvaluatedMonomials[E, M, F](poly, ambientGroup)
    val rootOrder = GenPerm.commonRootOrder(ambientGroup.generators)
    val nMonomials = monomials.length
    val nOperators = valueOf[F].nOperators
    val conj = GenPerm(
      Perm.fromImages(
        Vector.range(nMonomials, nMonomials + nOperators) ++ Vector.range(0, nMonomials)
      ), Phases.empty
    )
    val action = new GenPermFaithfulPermutationAction(nMonomials + nOperators, rootOrder)
    val fullDomainGenerators = ambientGroup.generators.map { g =>
      momentSetAction[E, M, F](monomials, g) |+| conj.inverse |+| g |+| conj
    }
    val coeffSeq = (for {
      mono <- monomials.iterator.toVector
      coeff = poly.normalForm.coeff(mono.normalForm: Mono[M, F])
      k <- 0 until rootOrder
      phase = Phase(k, rootOrder)
    } yield coeff * phase.toCyclo) ++ Seq.fill(nOperators * rootOrder)(Cyclo.zero)
    val partition = Partition.fromSeq(coeffSeq)
    val fullDomainSubgroupGenerators = Grp(fullDomainGenerators: _*) // construct group with full action
      .orderedPartitionStabilizer(action, partition) // find subgroup that fixes partition
      .smallGeneratingSet // find small subset of generators
    val subgroupGenerators = fullDomainSubgroupGenerators.map(g => (conj |+| g |+| conj.inverse).truncate(nOperators))
    Grp(subgroupGenerators: _*)
  }

}
