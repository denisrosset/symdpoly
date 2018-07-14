package net.alasc.symdpoly

import shapeless.Witness

package object evaluation {

  def pureStateSelfAdjoint[F <: free.MonoidDef.Aux[F] with Singleton]
  (M: generic.FreeBasedMonoidDef.Aux[F] with Singleton): FreeBasedEvaluator[M.type, F] = {
    implicit def wM: Witness.Aux[M.type] = M.witness
    implicit def wF: Witness.Aux[F] = (M.Free: F).witnessFree
    new FreeBasedEvaluator[M.type, F](Vector(evaluation.Equivalence.fullAdjoint[F]), true)
  }

  def pureState[F <: free.MonoidDef.Aux[F] with Singleton]
  (M: generic.FreeBasedMonoidDef.Aux[F] with Singleton): FreeBasedEvaluator[M.type, F] = {
    implicit def wM: Witness.Aux[M.type] = M.witness
    new FreeBasedEvaluator[M.type, F](Vector(), false)
  }

  def cyclic[F <: free.MonoidDef.Aux[F] with Singleton]
  (M: generic.FreeBasedMonoidDef.Aux[F] with Singleton): FreeBasedEvaluator[M.type, F] = {
    implicit def wM: Witness.Aux[M.type] = M.witness
    def F: F = M.Free
    implicit def wF: Witness.Aux[F] = (M.Free: F).witnessFree
    new FreeBasedEvaluator[M.type, F](Vector(evaluation.Equivalence.cyclic[F](x => true)), false)
  }


  def pptSelfAdjoint[F <: free.MonoidDef.Aux[F] with Singleton]
    (M: generic.FreeBasedMonoidDef.Aux[F] with Singleton)(partition: Seq[F#OpType]*): FreeBasedEvaluator[M.type, F] = {
    implicit def wM: Witness.Aux[M.type] = M.witness
    def F: F = M.Free
    implicit def wF: Witness.Aux[F] = (M.Free: F).witnessFree
    require(partition.flatten == F.operators, "The given partition must contain all operators")
    if (partition.size == 1) pureStateSelfAdjoint(M) else {
      val equivalences = partition.tail.map { opTypes =>
        evaluation.Equivalence.transpose((op: F#Op) => opTypes.exists(_.allInstances.contains(op)))
      }
      new FreeBasedEvaluator[M.type, F](equivalences :+ evaluation.Equivalence.fullAdjoint[F], true)
    }
  }

}
