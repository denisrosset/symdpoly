package net.alasc.symdpoly
package evaluation

import net.alasc.perms.Perm
import org.scalacheck.Arbitrary
import net.alasc.perms.default._

class GrpDecompositionSuite extends CommonSuite {
  implicit val arbPerm: Arbitrary[Perm] = Arbitrary(net.alasc.laws.Permutations.permForSize(6))
  import net.alasc.laws.Grps.arbGrp
  forAll(arbGrp[Perm].arbitrary) { grp =>
    val decomposition = GrpDecomposition(grp)
    def allElements(chain: List[Vector[Perm]]): Vector[Perm] = chain match {
      case hd :: tl =>
        for {
          h <- hd
          t <- allElements(tl)
        } yield h |+| t
      case Nil => Vector(Perm.id)
    }
    grp.iterator.toSet == allElements(decomposition.transversals).toSet
  }
}
