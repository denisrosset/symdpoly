package net.alasc.symdpoly.pretty

import scala.annotation.tailrec

import shapeless.HMap
import spire.syntax.cfor.cforRange

import cyclo.Cyclo
import scalin.immutable.Mat
import syntax._

/** Text format, for example to output on the console, or to write into text files. */
object Text extends Format {
  type Output = String
  implicit val cyclo: Pretty[Cyclo, Text.type] = Pretty.noSettings[Cyclo, Text.type](_.toString)
  implicit val int: Pretty[Int, Text.type] = Pretty.noSettings[Int, Text.type](_.toString)
  implicit def mat[A](implicit ev: Pretty[A, Text.type]): Pretty[Mat[A], Text.type] =
    new Pretty[Mat[A], Text.type] {
      def apply(mat: Mat[A])(implicit settings: HMap[Key.Relation]): String =
        if (settings.value(matAlignColumns)) {
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
        } else
          Seq.tabulate(mat.nRows)(r => Seq.tabulate(mat.nCols)(c => ev(mat(r, c))).mkString(" ")).mkString("\n")
    }
}
