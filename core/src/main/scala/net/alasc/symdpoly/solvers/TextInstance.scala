package net.alasc.symdpoly
package solvers

import java.io.{BufferedWriter, FileWriter, StringWriter, Writer}
import resource._

trait TextInstance {
  def writeData(writer: Writer): Unit
  def writeFile(filename: String): Unit = {
    val file = new java.io.File(filename)
    for {
      fileWriter <- managed(new FileWriter(file))
      bufferedWriter <- managed(new BufferedWriter(fileWriter))
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
