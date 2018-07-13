package net.alasc.symdpoly
package laws

import shapeless.Witness

import cyclo.Cyclos

import net.alasc.symdpoly.generic.FreeBasedMonoidDef
import org.scalacheck.{Arbitrary, Gen}

object Polys {

  // Methods and instances for randomized tests

  def genSinglePoly[M <: FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux, F <: free.MonoidDef.Aux[F] with Singleton]: Gen[Poly[M, F]] =
    for {
      c <- Cyclos.genSimpleCyclo
      mono <- Monos.gen[M, F]
    } yield Poly.single(mono, c)

  def genPoly[M <: FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux, F <: free.MonoidDef.Aux[F] with Singleton]: Gen[Poly[M, F]] =
    Gen.listOf(genSinglePoly[M, F]).map(_.foldLeft(Poly.zero[M, F])(_ + _))

  implicit def arbPoly[M <: FreeBasedMonoidDef.Aux[F] with Singleton:Witness.Aux, F <: free.MonoidDef.Aux[F] with Singleton]: Arbitrary[Poly[M, F]] =
    Arbitrary( genPoly[M, F] )

}
