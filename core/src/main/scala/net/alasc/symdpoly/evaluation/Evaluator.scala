package net.alasc.symdpoly.evaluation


import net.alasc.symdpoly.generic
import net.alasc.symdpoly.math.GenPerm
import shapeless.Witness
import spire.algebra.Order

import net.alasc.finite.Grp

trait Evaluator[M <: generic.MonoidDef with Singleton] { self =>
  val wE: Witness.Aux[self.type] = Witness.mkWitness(self)
  implicit def wM: Witness.Aux[M]
  def M: M = wM.value

  // optimization: set to true if reduceMono(a) == reduceMono(a.adjoint)
  def isSelfAdjoint: Boolean

  // we cache some instances that may not depend on G
  def evaluatedMonoOrder[G <: Grp[GenPerm] with Singleton]: Order[EvaluatedMono[self.type, M, G]]

  type ScratchPad
  def makeScratchPad: ScratchPad

  def apply(mono: M#Monomial): EvaluatedMono[self.type, M, M#TrivialGroup] = apply(mono, makeScratchPad)
  def apply(mono: M#Monomial, pad: ScratchPad): EvaluatedMono[self.type, M, M#TrivialGroup] = apply(mono, M.trivialGroup: M#TrivialGroup, pad)
  def apply(mono: M#Monomial, group: Grp[GenPerm]): EvaluatedMono[self.type, M, group.type] = apply(mono, group, makeScratchPad)
  def apply(mono: M#Monomial, group: Grp[GenPerm], pad: ScratchPad): EvaluatedMono[self.type, M, group.type]

  def apply(poly: M#Polynomial)(implicit d: DummyImplicit): EvaluatedPoly[self.type, M, M#TrivialGroup] = apply(poly, makeScratchPad)
  def apply(poly: M#Polynomial, pad: ScratchPad)(implicit d: DummyImplicit): EvaluatedPoly[self.type, M, M#TrivialGroup] = apply(poly, M.trivialGroup, pad)
  def apply(poly: M#Polynomial, group: Grp[GenPerm])(implicit d: DummyImplicit): EvaluatedPoly[self.type, M, group.type] = apply(poly, group, makeScratchPad)
  def apply(poly: M#Polynomial, group: Grp[GenPerm], pad: ScratchPad)(implicit d: DummyImplicit): EvaluatedPoly[self.type, M, group.type]
}
