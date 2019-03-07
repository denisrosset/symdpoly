package net.alasc.symdpoly

import cats.evidence.Is

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3
import shapeless.Witness
import spire.algebra.{Action, Eq, FieldAssociativeAlgebra, Involution}
import spire.math.{Rational, Searching}
import spire.syntax.cfor._
import spire.syntax.involution._
import spire.syntax.order._
import cyclo.Cyclo
import metal.mutable.{HashMap => MMap}
import metal.syntax._
import net.alasc.symdpoly.free.{MutablePoly, MutableWord}
import net.alasc.symdpoly.generic._
import net.alasc.symdpoly.math.GenPerm
import org.scalacheck.{Arbitrary, Gen}
import org.typelevel.discipline.Predicate
import syntax.all._
import instances.all._

abstract class GenPoly[M <: MonoidDef with Singleton] {
  def nTerms: Int
  def monomial(i: Int): M#Monomial
  def coeff(i: Int): Cyclo
  def coeff(mono: M#Monomial): Cyclo
  def string(leftBracket: String = "", rightBracket: String = ""): String
}
