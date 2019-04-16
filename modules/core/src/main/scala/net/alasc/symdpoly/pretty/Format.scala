package net.alasc.symdpoly.pretty

import shapeless.HMap
import syntax._

/** An output format for pretty printing. */
trait Format { self =>
  type Output
  def makePrinter[A](f: A => Output): Printer[A, self.type] = new Printer[A, self.type] {
    def apply(a: A): Output = f(a)
  }
}

object Format {
  type Aux[O] = Format { type Output = O }
}
