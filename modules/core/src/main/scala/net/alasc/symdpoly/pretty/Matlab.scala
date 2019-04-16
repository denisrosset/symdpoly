package net.alasc.symdpoly.pretty

import spire.math.{Rational, SafeLong}

import scalin.immutable.{Mat, Vec}

import net.alasc.symdpoly.math.Phase

/** Matlab source code format. */
object Matlab extends Format {

  case class Statement(val name: String, val value: Output)

  object Statement {
    implicit def convert[A](pair: (String, A))(implicit printer: Printer[A, Matlab.type]): Statement =
      new Statement(pair._1, printer(pair._2))
  }

  implicit def statement: Printer[Statement, Matlab.type] = makePrinter {
    case Statement(name, value) => s"$name = $value\n"
  }

  def statements(pairs: Statement*): Seq[Statement] = pairs

  implicit def statements: Printer[Iterable[Statement], Matlab.type] = makePrinter(st => st.map(statement.apply).mkString("\n"))

  type Output = String
  implicit val int: Printer[Int, Matlab.type] = makePrinter(_.toString)
  implicit val phase: Printer[Phase, Matlab.type] = makePrinter { (phase: Phase) =>
    if (phase.isOne) "1"
    else if (phase.isMinusOne) "-1"
    else if (phase.isI) "1i"
    else if (phase.isMinusI) "-1i"
    else {
      val rot = Rational(2*phase.k, phase.n)
      s"exp(${rot.numerator}i*pi/${rot.denominator})"
      }
    }
  implicit def mat[A](implicit ev: Printer[A, Matlab.type]): Printer[Mat[A], Matlab.type] =
    makePrinter { mat =>
      Seq.tabulate(mat.nRows) {
        r => Seq.tabulate(mat.nCols)(c => ev(mat(r, c))).mkString(",")
      }.mkString("[", "\n", "]")
    }
}
