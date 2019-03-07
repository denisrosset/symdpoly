package net.alasc.symdpoly
package laws

import shapeless.Witness
import spire.NoImplicit
import net.alasc.symdpoly.free.MutableWord
import net.alasc.symdpoly.generic.{FreeBasedMono, FreeBasedMonoidDef}
import net.alasc.symdpoly.math.Phase
import org.scalacheck.{Arbitrary, Gen}

object Monos {

  // Generators

  def genNonZeroFree[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux]: Gen[FreeBasedMono[F, F]] =
    for {
      phase <- Phase.genForDenominator(valueOf[F].cyclotomicOrder)
      length <- Gen.choose(0, 8)
      indices <- Gen.containerOfN[Array, Int](length, Gen.choose(0, valueOf[F].nOperators - 1))
    } yield new FreeBasedMono[F, F](new MutableWord[F](phase, length, indices, false))

  // Generator for a random free monomial
  def genFree[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux]: Gen[FreeBasedMono[F, F]] =
    Gen.frequency(1 -> Gen.const(FreeBasedMono.zero[F, F]), 10 -> genNonZeroFree[F])

  def gen[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](implicit wM: Witness.Aux[M]): Gen[FreeBasedMono[M, F]] = {
    implicit def wF: Witness.Aux[F] = (wM.value: M).witnessFree
    genFree[F].map(mono => (wM.value: M).quotient(mono))
  }

  implicit def arb[
    M <: FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux
  ](implicit ev: NoImplicit[F =:= M]): Arbitrary[FreeBasedMono[M, F]] = Arbitrary(gen[M, F])

  implicit def arbFree[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux]: Arbitrary[FreeBasedMono[F, F]] =
    Arbitrary(genFree[F])

}
