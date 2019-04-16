package net.alasc.symdpoly
package pretty

import shapeless.Witness
import spire.math.Rational

import scalin.immutable.{Mat, Vec}

import net.alasc.symdpoly.evaluation.Evaluator
import net.alasc.symdpoly.generic.SingleMoment
import net.alasc.symdpoly.math.Phase
import net.alasc.symdpoly.syntax.phased._

/** Mathematica source code format. */
object Mathematica extends Format {

  case class Statement(val name: String, val value: Output)

  object Statement {
    implicit def convert[A](pair: (String, A))(implicit printer: Printer[A, Mathematica.type]): Statement =
      new Statement(pair._1, printer(pair._2))
  }

  implicit def statement: Printer[Statement, Mathematica.type] = makePrinter {
    case Statement(name, value) => s"$name := $value\n"
  }

  def statements(pairs: Statement*): Seq[Statement] = pairs

  implicit def statements: Printer[Iterable[Statement], Mathematica.type] = makePrinter(st => st.map(statement.apply).mkString("\n"))

  type Output = String
  implicit val int: Printer[Int, Mathematica.type] = makePrinter(_.toString)
  implicit val phase: Printer[Phase, Mathematica.type] = makePrinter { (phase: Phase) =>
    if (phase.isOne) "1"
    else if (phase.isMinusOne) "-1"
    else if (phase.isI) "I"
    else if (phase.isMinusI) "-I"
    else {
      val rot = Rational(2*phase.k, phase.n)
      s"Exp[${rot.numerator}I Pi/${rot.denominator}]"
    }
  }
  implicit def mat[A](implicit ev: Printer[A, Mathematica.type]): Printer[Mat[A], Mathematica.type] =
    makePrinter { mat =>
      Seq.tabulate(mat.nRows) {
        r => Seq.tabulate(mat.nCols)(c => ev(mat(r, c))).mkString(",")
      }.mkString("{\n  {", "},\n  {", "}\n}")
    }

  implicit def vec[A](implicit ev: Printer[A, Mathematica.type]): Printer[Vec[A], Mathematica.type] =
    makePrinter { vec => vec.toIndexedSeq.map(ev.apply).mkString("{", ",", "}") }

  implicit def freeMono[F <: free.MonoDef with Singleton: Witness.Aux](implicit ev: Printer[F#Op, Mathematica.type]): Printer[F#MonoType, Mathematica.type] =
    makePrinter { mono =>
      def F: F = valueOf[F]
      if (F.monoMultiplicativeBinoid.isZero(mono)(F.monoOrder)) "0"
      else if (mono.length == 0) phase.apply(mono.phase)
      else if (mono.phase.isOne) Seq.tabulate(mono.length)(i => ev.apply(mono(i))).mkString("**")
      else phase.apply(F.monoPhased.phaseOffset(mono)) + " " + freeMono.apply(F.monoPhased.phaseCanonical(mono))
    }

  implicit def mono[M <: quotient.MonoDef.Aux[F] with Singleton: Witness.Aux, F <: free.MonoDef.Aux[F] with Singleton](implicit ev: Printer[F#Op, Mathematica.type]): Printer[M#MonoType, Mathematica.type] =
    makePrinter { mono =>
      implicit def witnessF: Witness.Aux[F] = valueOf[M].witnessFree
      freeMono.apply(mono.normalForm)
    }

  implicit def singleMoment[E <: Evaluator.Aux[M] with Singleton, M <: generic.MonoDef with Singleton](implicit ev: Printer[M#MonoType, Mathematica.type]): Printer[generic.SingleMoment[E, M], Mathematica.type] =
    makePrinter { sm =>
      ev.apply(sm.normalForm)
    }
}
