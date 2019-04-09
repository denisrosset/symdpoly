package net.alasc.symdpoly
package laws

import shapeless.Witness
import spire.NoImplicit

import net.alasc.symdpoly.free.MutableWord
import net.alasc.symdpoly.freebased.Mono
import net.alasc.symdpoly.math.Phase
import org.scalacheck.{Arbitrary, Gen}

object Monos {

  // Generators

  def genNonZeroFree[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux]: Gen[Mono[F, F]] =
    for {
      phase <- Phase.genForDenominator(valueOf[F].cyclotomicOrder)
      length <- Gen.choose(0, 8)
      indices <- Gen.containerOfN[Array, Int](length, Gen.choose(0, valueOf[F].nOperators - 1))
    } yield new Mono[F, F](new MutableWord[F](phase, length, indices, MutableWord.Immutable, -1))

  // Generator for a random free monomial
  def genFree[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux]: Gen[Mono[F, F]] =
    Gen.frequency(1 -> Gen.const(Mono.zero[F, F]), 10 -> genNonZeroFree[F])

  def gen[M <: freebased.MonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](implicit wM: Witness.Aux[M]): Gen[Mono[M, F]] = {
    implicit def wF: Witness.Aux[F] = (wM.value: M).witnessFree
    genFree[F].map(mono => (wM.value: M).quotient(mono))
  }

  implicit def arb[
    M <: freebased.MonoidDef.Aux[F] with Singleton:Witness.Aux,
    F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux
  ](implicit ev: NoImplicit[F =:= M]): Arbitrary[Mono[M, F]] = Arbitrary(gen[M, F])

  implicit def arbFree[F <: free.MonoidDef.Aux[F] with Singleton:Witness.Aux]: Arbitrary[Mono[F, F]] =
    Arbitrary(genFree[F])

}
