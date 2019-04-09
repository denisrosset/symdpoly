package net.alasc.symdpoly

import net.alasc.finite.Grp
import shapeless.Witness

package object evaluation {

  private[this] def false1[A](a: A): Boolean = false
  private[this] def true1[A](a: A): Boolean = true
  private[this] def false2[A, B](a: A, b: B): Boolean = false
  private[this] def true2[A, B](a: A, b: B): Boolean = true

  /** Constructs an equivalence relation from various components.
    *
    * This variant of the construction does not preserve symmetries.
    *
    * @param components            Components of the equivalence relation
    */
  def symmetryNotPreserving[
  M <: freebased.MonoidDef.Aux[F] with Singleton:Witness.Aux,
  F <: free.MonoidDef.Aux[F] with Singleton
  ](components: FreeBasedComponent[M, F]*): Equivalence[M] =
    symmetryPreserving[M, F](false1 _, false2 _, components: _*)

  /** Constructs an equivalence relation from various components.
    *
    * This variant of the construction recovers symmetries in an inefficient manner.
    *
    * @param components            Components of the equivalence relation
    * @param permutationCompatible Returns whether a permutation is compatible with the equivalence relation.
    *                              Note that phases are set to one in the permutation passed for the check.
    */

  def symmetryPreserving[
    M <: freebased.MonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](permutationCompatible: M#PermutationType => Boolean,
    components: FreeBasedComponent[M, F]*): Equivalence[M] =
    symmetryPreserving[M, F](permutationCompatible, true2 _, components: _*)

  /** Constructs an equivalence relation from various components.
    *
    * The parameters `permutationCompatible` and `actionCompatible` are used to find the symmetry subgroup
    * compatible with the equivalence relation in an efficient manner.
    *
    * @param components            Components of the equivalence relation
    * @param actionCompatible      A Boolean function with two parameters (preimage, image).
    *                              Returns false when there is no permtuation g such that
    *                                           image = preimage <|+| g.
    *                              False positives are allowed (i.e. returning true when the correct answer is false).
    *                              But not false negatives (returning false is a precise statement).
    * @param permutationCompatible Returns whether a permutation is compatible with the equivalence relation.
    *                              Note that phases are set to one in the permutation passed for the check.
    */
  def symmetryPreserving[
    M <: freebased.MonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](permutationCompatible: M#PermutationType => Boolean,
    actionCompatible: (F#Op, F#Op) => Boolean,
    components: FreeBasedComponent[M, F]*): Equivalence[M] = new ComponentEquivalence[M, F](components, permutationCompatible, actionCompatible)

  /** Returns an equivalence relation under cyclic permutation of operators. */
  def cyclic[F <: free.MonoidDef.Aux[F] with Singleton](M: freebased.MonoidDef.Aux[F]): Equivalence[M.type] =
    symmetryNotPreserving(Component.cyclic[M.type, F](true1))

  def cyclicReal[F <: free.MonoidDef.Aux[F] with Singleton](M: freebased.MonoidDef.Aux[F]): Equivalence[M.type] =
    symmetryNotPreserving(Component.cyclic[M.type, F](true1), Component.transpose[M.type, F](true1))

  /** Equivalence under the adjoint operation. */
  def real[M <: generic.MonoidDef with Singleton: Witness.Aux]: Equivalence[M] = AdjointEquivalence[M]()

  /** Returns a trivial equivalence relation, where the equivalence class of m is Set(m). */
  def trivial[M <: generic.MonoidDef with Singleton: Witness.Aux](): Equivalence[M] = TrivialEquivalence[M]

  def partialTransposes[
    F <: free.MonoidDef.Aux[F] with Singleton
  ](M: freebased.MonoidDef.Aux[F])(blocks: F#OpEnum*): Equivalence[M.type] = {
    implicit def witnessM: Witness.Aux[M.type] = M.witness
    implicit def witnessF: Witness.Aux[F] = M.witnessFree
    val map: Map[F#Op, Int] = blocks.zipWithIndex.flatMap {
      case (block, i) => block.allInstances.map(op => (op, i))
    }.groupBy(_._1).mapValues {
      case Seq((op: F#Op, i: Int)) => i
      case _ => throw new IllegalArgumentException("Blocks must be disjoint")
    }
    val groupIndex = valueOf[F].opIndexMap.elements.map( op => map.getOrElse(op, -1) ).toArray[Int]
    new TransposesEquivalence[M.type, F](groupIndex)
  }

  sealed trait LetterEquivalence
  case object Transpose extends LetterEquivalence
  case object Cyclic extends LetterEquivalence

  def partiallyCommutative[F <: free.MonoidDef.Aux[F] with Singleton](M: quotient.MonoidDef.Aux[F], parts: Map[Set[F#Op], Boolean])(groups: (F#OpEnum, LetterEquivalence)*): Equivalence[M.type] = {
    ???
  }

  def partiallyCommutative[F <: free.MonoidDef.Aux[F] with Singleton](M: quotient.MonoidDef.Aux[F], parts: Set[Set[F#Op]])(groups: (F#OpEnum, LetterEquivalence)*): Equivalence[M.type] = {
    val iq = new PartiallyCommutative.InvestigatedQuotient[M.type, F]()(M.witness)
    require(iq.verify(parts))
    partiallyCommutative[F](M, iq.computeMap(parts))(groups: _*)
  }


  def partiallyCommutative[F <: free.MonoidDef.Aux[F] with Singleton](M: quotient.MonoidDef.Aux[F])(groups: (F#OpEnum, LetterEquivalence)*): Equivalence[M.type] = {
    val iq = new PartiallyCommutative.InvestigatedQuotient[M.type, F]()(M.witness)
    partiallyCommutative[F](M, iq.ansatzFromRules)(groups: _*)
  }

}
