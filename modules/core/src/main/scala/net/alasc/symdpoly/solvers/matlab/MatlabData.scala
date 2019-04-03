package net.alasc.symdpoly
package solvers
package matlab

import java.util.{ArrayList => JavaArrayList, Collection => JavaCollection}

import spire.syntax.cfor._

import scalin.immutable.{Mat, Vec}

import syntax.phased._
import spire.std.double._

import fs2.Sink
import io.tmos.arm.ArmMethods._
import us.hebi.matlab.mat.types.{MatFile, Sinks}

trait MatlabFormat {

  def data: MatFile

  def writeFile(filename: String): Unit =
    for (sink <- manage(Sinks.newStreamingFile(filename))) {
      data.writeTo(sink)
    }

}
