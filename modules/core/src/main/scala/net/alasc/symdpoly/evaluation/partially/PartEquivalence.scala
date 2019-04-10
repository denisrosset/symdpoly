package net.alasc.symdpoly.evaluation.partially

sealed trait PartEquivalence
case object Transpose extends PartEquivalence
case object Cyclic extends PartEquivalence
