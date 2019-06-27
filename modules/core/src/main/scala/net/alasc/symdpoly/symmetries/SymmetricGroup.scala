package net.alasc.symdpoly.symmetries

import scala.annotation.tailrec

import spire.algebra.Group
import spire.math.SafeLong

import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.symdpoly.algebra.Morphism
import spire.syntax.euclideanRing._

import net.alasc.perms.default._
import net.alasc.std.any._
import spire.std.tuples._
import cats.syntax.contravariant._
import net.alasc.symdpoly.instances.invariant._
import spire.syntax.action._

import net.alasc.algebra.PermutationAction
import net.alasc.syntax.all._

object SymmetricGroup {
  import net.alasc.named.Cyclic.shift
  import net.alasc.named.Symmetric.transposition

  /** Presentation of the symmetric group S(n) with two generators s and t such that
    * s^n = t^2 = (st)^(n-1) = [t,s^j]^2 = 1 for j = 2,..,n/2
    *
    * see (2.1) in http://www.ams.org/journals/tran/2003-355-05/S0002-9947-03-03040-X/S0002-9947-03-03040-X.pdf
    *
    * and n >= 3.
    */
  case class Presentation(n: Int, s: Perm, t: Perm) {
    require(n >= 3)
    def sCanonical: Perm = shift(n)
    def tCanonical: Perm = transposition(0, 1)
  }

  /** Finds x such that x! = n. Returns None if n is not a factorial. */
  def inverseFactorial(n: SafeLong): Option[Int] = {
    // divides successively by 2,3,4,5 etc...
    // Invariant: rem = n / 2 / 3 ... / d - 1
    @tailrec def rec(rem: SafeLong, d: Int): Option[Int] =
      if (!rem.isValidInt || rem.toInt > d) {
        val (q, m) = rem equotmod SafeLong(d)
        if (m.isZero) rec(q, d + 1) else None
      } else if (rem == d) Some(d)
      else None
    rec(n, 2)
  }

  /** Tries to find a presentation for `grp` a symmetric group. For now, works only if an orbit of `grp` is the natural
    * permutation representation of a symmetric group. */
  def tryRecognize(grp: Grp[Perm]): Option[Presentation] = inverseFactorial(grp.order) flatMap { n =>
    @tailrec def rec(rest: Grp[Perm], skipped: Int): Option[Presentation] =
      if (rest.isTrivial) None else {
        val phi = Orbit.splitNonTransitive(rest)
        val split: Grp[(Perm, Perm)] = phi.grpImage(grp)
        val grpLeft = Grp(split.generators.map(_._1): _*)
        val grpRight = Grp(split.generators.map(_._1): _*)
        val orbitSize = grpLeft.largestMovedPoint.fold(0)(_ + 1)
        if (orbitSize == n && grpLeft.order == grp.order) {

          val actionInOrbit = (Perm.algebra: PermutationAction[Perm]).contramap( (perm: Perm) => Perm.fromImageFun(orbitSize, i => ((i + skipped) <|+| perm) - skipped) )
          for {
            s <- grp.findSameAction(actionInOrbit, shift(n)).toOption
            t <- grp.findSameAction(actionInOrbit, transposition(0, 1)).toOption
          } yield Presentation(n, s, t)
        } else rec(grpRight, skipped + orbitSize)
      }
    if (n <= 2) None else rec(grp, 0)
  }

}
