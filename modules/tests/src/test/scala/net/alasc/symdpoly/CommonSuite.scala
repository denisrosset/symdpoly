package net.alasc.symdpoly

import cyclo.RealCyclo
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.FunSuite
import shapeless.Witness
import spire.algebra.{CRing, MultiplicativeMonoid, Ring}
import spire.syntax.ring._
import org.scalatest.exceptions.DiscardedEvaluationException
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}
import org.typelevel.discipline.Predicate
import org.typelevel.discipline.scalatest.Discipline
import scala.collection.immutable.BitSet

/**
  * An opinionated stack of traits to improve consistency and reduce
  * boilerplate.
  */
trait CommonSuite extends FunSuite with Matchers
  with PropertyChecks
  with Discipline
  with spire.syntax.AllSyntax with spire.std.AnyInstances {

  def noShrink[T] = Shrink[T](_ => Stream.empty)

}
