package net.alasc.symdpoly.pretty

import scala.annotation.tailrec

import spire.syntax.cfor._

import cyclo.Cyclo
import scalin.immutable.Mat

/** Text format, for example to output on the console, or to write into text files. */
object Text extends Format {
  type Output = String
  implicit val cyclo: Printer[Cyclo, Text.type] = makePrinter(_.toString)
  implicit val int: Printer[Int, Text.type] = makePrinter(_.toString)
  implicit def mat[A](implicit ev: Printer[A, Text.type]): Printer[Mat[A], Text.type] =
    new Printer[Mat[A], Text.type] {
      def apply(mat: Mat[A]): String = {
        @tailrec def colWidth(c: Int, r: Int = 0, width: Int = 0): Int =
          if (r >= mat.nRows) width else colWidth(c, r + 1, spire.math.max(width, ev(mat(r, c)).length + 2))

        val colWidths = Array.tabulate(mat.nCols)(c => colWidth(c))
        val rv = new scala.StringBuilder
        cforRange(0 until mat.nRows) { r =>
          cforRange(0 until mat.nCols) { c =>
            val cell = ev(mat(r, c))
            rv.append(" " * (colWidths(c) - cell.length))
            rv.append(cell)
          }
          if (r + 1 < mat.nRows) rv.append("\n")
        }
        rv.toString
      }
    }
}
