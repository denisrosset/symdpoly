package net.alasc.symdpoly
package solvers

import java.io.{BufferedWriter, FileWriter, StringWriter, Writer}
import io.tmos.arm.ArmMethods._

trait TextFormat {
  def writeData(writer: Writer): Unit
  def writeFile(filename: String): Unit = {
    val file = new java.io.File(filename)
    for {
      fileWriter <- manage(new FileWriter(file))
      bufferedWriter <- manage(new BufferedWriter(fileWriter))
    } {
      writeData(bufferedWriter)
    }
  }

  def data: String = {
    val sw = new StringWriter
    writeData(sw)
    sw.toString
  }
}
