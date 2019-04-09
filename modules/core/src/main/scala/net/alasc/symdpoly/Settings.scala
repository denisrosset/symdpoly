package net.alasc.symdpoly

import java.io.PrintStream

import scala.util.DynamicVariable

import spire.algebra.Order
import spire.std.int._

sealed abstract class Verbosity(val value: Int)

object Verbosity {
  object Quiet extends Verbosity(0)
  object Normal extends Verbosity(1)
  object Verbose extends Verbosity(2)
  implicit val order: Order[Verbosity] = Order.by(_.value)
}

object Settings {
  private[this] val _verbosity = new DynamicVariable[Verbosity](Verbosity.Normal)
  private[this] val _useProgressBar = new DynamicVariable[Boolean](true)
  private[this] val _err = new DynamicVariable[PrintStream](java.lang.System.err)
  private[this] val _optimize = new DynamicVariable[Boolean](true)
  def verbosity: Verbosity = _verbosity.value
  def useProgressBar: Boolean = _useProgressBar.value
  def err: PrintStream = _err.value
  def optimize: Boolean = _optimize.value
  def withVerbosity[T](verbosity: Verbosity)(thunk: => T): T =
    _verbosity.withValue(verbosity)(thunk)
  def withUseProgressBar[T](useProgressBar: Boolean)(thunk: => T): T =
    _useProgressBar.withValue(useProgressBar)(thunk)
  def withErr[T](err: PrintStream)(thunk: => T): T =
    _err.withValue(err)(thunk)
  def withOptimize[T](optimize: Boolean)(thunk: => T): T =
    _optimize.withValue(optimize)(thunk)
}
