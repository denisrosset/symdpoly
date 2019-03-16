package net.alasc.symdpoly
package sdp

import scala.collection.immutable.HashSet

import shapeless.Witness

import net.alasc.symdpoly.evaluation.Evaluator
import net.alasc.symdpoly.util.OrderedSet
import spire.syntax.action._
import spire.syntax.group._
import syntax.phased._
import cyclo.Cyclo

case class BasisTerm(basisIndex: Int, realPart: Double, imagPart: Double)

object BasisTerm {

  implicit class SeqBasisTerm(val terms: Seq[BasisTerm]) {
    def collapse: Seq[BasisTerm] = terms.groupBy(_.basisIndex).values.map {
      case Seq(BasisTerm(d, rp, ip), tl@_*) => BasisTerm(d, tl.map(_.realPart).foldLeft(rp)(_ + _), tl.map(_.imagPart).foldLeft(rp)(_ + _))
    }.toSeq
  }

}
