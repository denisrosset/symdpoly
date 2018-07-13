package net.alasc.symdpoly
package laws

import net.alasc.symdpoly.math.{GenPerm, Phases}
import org.scalacheck.{Arbitrary, Gen}
import org.typelevel.discipline.Predicate

object GenPerms {
  def genPhases: Gen[Phases] = for {
    n <- Gen.choose(0, 20)
    phases <- Gen.containerOfN[Vector, Phase](n, Phase.gen)
  } yield Phases(phases.zipWithIndex.map(_.swap): _*)

  implicit val arbPhases: Arbitrary[Phases] = Arbitrary(genPhases)
  implicit val predPhases: Predicate[Phases] = Predicate(x => true)

  def genGenPerm: Gen[GenPerm] = for {
    perm <- net.alasc.laws.Permutations.permForSize(20)
    phases <- genPhases
  } yield GenPerm(perm, phases)

  implicit val arbGenPerm: Arbitrary[GenPerm] = Arbitrary(genGenPerm)
  implicit val predGenPerm: Predicate[GenPerm] = Predicate(x => true)
}
