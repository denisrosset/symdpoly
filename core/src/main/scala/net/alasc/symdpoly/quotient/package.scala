package net.alasc.symdpoly

import net.alasc.symdpoly.generic.FreeBasedMono
import shapeless.Witness
import spire.syntax.eq._

package object quotient {

  type Rules[F <: free.MonoidDef.Aux[F] with Singleton] = Seq[(FreeBasedMono[F, F], FreeBasedMono[F, F])]

  def pairs[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux](rules: PairSubstitutions[F]): Rules[F] = for {
    op1 <- valueOf[F].opIndexMap.elements
    op2 <- valueOf[F].opIndexMap.elements
    lhs = op1 * op2
    rhs = rules(op1, op2) if lhs =!= rhs
  } yield (lhs -> rhs)

  def commutative[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux]: Rules[F] = for {
    index1 <- 0 until valueOf[F].nOperators
    index2 <- 0 until index1
    op1 = valueOf[F].opFromIndex(index1)
    op2 = valueOf[F].opFromIndex(index2)
    lhs = op1 * op1
    rhs = op2 * op1
  } yield (lhs -> rhs)


  def rules[F <: free.MonoidDef.Aux[F] with Singleton](pairs: (FreeBasedMono[F, F], FreeBasedMono[F, F])*): Rules[F] = pairs

}
