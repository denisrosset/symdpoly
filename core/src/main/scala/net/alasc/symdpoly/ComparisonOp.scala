package net.alasc.symdpoly

/** Comparison operators used to express constraints. */
sealed trait ComparisonOp

object ComparisonOp {

  case object LE extends ComparisonOp

  case object GE extends ComparisonOp

  case object EQ extends ComparisonOp

}