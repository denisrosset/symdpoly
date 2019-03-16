package net.alasc.symdpoly

import scalin.immutable.{Mat, Vec}

sealed trait Solution

case class OptimumFound(primalObjective: Option[Double], dualObjective: Double) extends Solution

case class Failure(status: String) extends Solution
