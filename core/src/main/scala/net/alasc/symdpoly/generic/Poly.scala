package net.alasc.symdpoly
package generic

import shapeless.Witness
import spire.math.Rational

import cyclo.Cyclo

abstract class Poly[M <: generic.MonoidDef with Singleton:Witness.Aux] { lhs: M#Polynomial =>

  def nTerms: Int
  def monomial(i: Int): M#Monomial
  def coeff(i: Int): Cyclo
  def coeff(mono: M#Monomial): Cyclo
  def string(leftBracket: String = "", rightBracket: String = ""): String

  def *(rhs: Int): M#Polynomial
  def *(rhs: Rational): M#Polynomial
  def *(rhs: Cyclo): M#Polynomial

  def /(rhs: Int): M#Polynomial
  def /(rhs: Rational): M#Polynomial
  def /(rhs: Cyclo): M#Polynomial

}
