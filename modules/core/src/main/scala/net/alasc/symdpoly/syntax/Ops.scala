package net.alasc.symdpoly
package syntax

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import net.alasc.symdpoly.algebra.Phased
import net.alasc.symdpoly.math.Phase
import spire.algebra.{Eq, Involution, VectorSpace}
import spire.macros.Ops
import spire.syntax.vectorSpace._
import spire.syntax.involution._

import net.alasc.symdpoly.pretty.{Format, Printer, Text}

/** Syntax enrichment for phased. */
final class PhasedOps[A](lhs: A)(implicit ev: Phased[A]) {
  def phaseOffset(): Phase = macro Ops.unop[Phase]
  def phaseCanonical(): A = macro Ops.unop[A]
}

/** Syntax for pretty printing operations. */
class PrinterOps[A](val a: A) {

  /** Writes a pretty printed description to file
    *
    * @param filename Name of the file to write
    * @param format   Textual format to use
    */
  def prettyWrite[F <: Format.Aux[String] with Singleton](filename: String, format: F)(implicit ev: Printer[A, F]): Unit =
    Files.write(Paths.get(filename), pretty(format: F).getBytes(StandardCharsets.UTF_8))

  /** Writes a pretty printed description to file using the default [[net.alasc.symdpoly.pretty.Text]] format.
    *
    * @param filename Name of the file to write
    */
  def prettyWrite(filename: String)(implicit ev: Printer[A, Text.type]): Unit =
    prettyWrite[Text.type](filename, Text)

  /** Pretty prints to the standard output
    *
    * @param format Textual format to use
    */
  def prettyPrint[F <: Format.Aux[String] with Singleton](format: F)(implicit ev: Printer[A, F]): Unit =
    println(pretty(format: F))

  /** Pretty prints to the standard output using the default [[Text]] format. */
  def prettyPrint()(implicit ev: Printer[A, Text.type]): Unit = prettyPrint[Text.type](Text)

  /** Formats the object using the given format.
    *
    * @param format Format to use
    */
  def pretty[F <: Format with Singleton](format: F)(implicit ev: Printer[A, F]): F#Output = ev(a)

  def pretty()(implicit ev: Printer[A, Text.type]): Text.Output = pretty[Text.type](Text)

}
