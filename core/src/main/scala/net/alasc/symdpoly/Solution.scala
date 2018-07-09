package net.alasc.symdpoly

import scalin.immutable.{Mat, Vec}

sealed trait Solution

case class OptimumFound(primalObjective: Option[Double], dualObjective: Double, X: Option[Mat[Double]], y: Vec[Double]) extends Solution

case class Failure(status: String) extends Solution
