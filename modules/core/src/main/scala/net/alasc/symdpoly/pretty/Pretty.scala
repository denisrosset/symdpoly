package net.alasc.symdpoly
package pretty

import java.io.File
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import scala.annotation.tailrec

import shapeless.{HMap, Witness}

import cyclo.Cyclo
import scalin.immutable.Mat
import spire.syntax.cfor._

import syntax._

/** Describes the ability to pretty print instances of A in the format F. */
trait Pretty[A, F <: Format with Singleton] {
  def apply(a: A)(implicit settings: HMap[Key.Relation]): F#Output
}

object Pretty {
  /** Returns an implicit instance of Pretty. */
  def apply[A, F <: Format with Singleton](implicit ev: Pretty[A, F]): Pretty[A, F] = ev
  /** Helper factory method to construct a Pretty instance that does not use settings. */
  def noSettings[A, F <: Format with Singleton](f: A => F#Output): Pretty[A, F] = new Pretty[A, F] {
    def apply(a: A)(implicit settings: HMap[Key.Relation]): F#Output = f(a)
  }
}
