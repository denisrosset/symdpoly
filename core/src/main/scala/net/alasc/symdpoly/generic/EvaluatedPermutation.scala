package net.alasc.symdpoly
package generic

import shapeless.Witness

final class EvaluatedPermutation[
  E <: generic.Evaluator[M] with Singleton: Witness.Aux,
  M <: generic.MonoidDef with Singleton
](val permutation: M#Permutation) {

  def E: E = valueOf[E]

  override def toString: String = permutation.toString

  override def hashCode: Int = permutation.hashCode

  override def equals(any: Any): Boolean = any match {
    case that: EvaluatedPermutation[E, M] => (this.E eq that.E) && (this.permutation == that.permutation)
    case _ => false
  }

}

object EvaluatedPermutation {
/*
  class GrpPermutationsOps[
  M <: generic.MonoidDef with Singleton: Witness.Aux,
  G <: Permutation[M]:ClassTag:Eq:FaithfulPermutationActionBuilder:Group
  ](grp: Grp[G]) {

    def M: M = valueOf[M]

    def allElementsOf[E <: Evaluator[M] with Singleton: Witness.Aux](poly: EvaluatedPoly[E, M])(implicit action: Action[EvaluatedMono[E, M], G]): OrderedSet[EvaluatedMono[E, M]] = {
      implicit def phasedEvaluatedMono: Phased[EvaluatedMono[E, M]] = valueOf[E].evaluatedMonoPhased
      val monomials: Set[EvaluatedMono[E, M]] = for {
        (g: G) <- grp.iterator.toSet
        i <- 0 until poly.nTerms
      } yield action.actr(poly.monomial(i), g).phaseCanonical
      val array = monomials.toArray
      spire.math.Sorting.quickSort(array)
      new OrderedSet(array.map(_.asInstanceOf[AnyRef]))
    }

    def leavesInvariant[E <: Evaluator[M] with Singleton: Witness.Aux](poly: EvaluatedPoly[E, M])(implicit action: Action[EvaluatedMono[E, M], G]): Grp[G] = {
      val monomials = allElementsOf[E](poly)(implicitly, action)
      val order = M.cyclotomicOrder
      val monomialsAction = new PermutationAction[G] {
        def isFaithful: Boolean = false
        def findMovedPoint(g: G): NNOption = {
          cforRange(0 until monomials.length * order) { i =>
            if (actr(i, g) != i) return NNSome(i)
          }
          NNNone
        }
        def movedPointsUpperBound(g: G): NNOption = NNSome(monomials.length * order - 1)
        def actr(p: Int, g: G): Int = {
          val phase = Phase(p % order, order)
          val index = p / order
          val res = monomials(index) <|+| g
          val canonical = res.phaseCanonical
          val newPhase = res.phaseOffset * phase
          val newIndex = monomials.indexOf(canonical)
          newIndex * order + newPhase.numeratorIn(order)
        }
        def actl(g: G, p: Int): Int = actr(p, g.inverse)
      }
      val nMonomials = monomials.length
      val coeffSeq = for {
        mono <- monomials.iterator.toVector
        coeff = poly.normalForm.coeff(mono.normalForm: M#Monomial)
        k <- 0 until order
        phase = Phase(k, order)
      } yield coeff * phase.toCyclo
      val partition = Partition.fromSeq(coeffSeq)
      val stabilizer = grp.orderedPartitionStabilizer(monomialsAction, partition)
      val generators = stabilizer.smallGeneratingSet
      Grp.fromGeneratorsAndOrder(generators, stabilizer.order)
    }

  }
*/
}