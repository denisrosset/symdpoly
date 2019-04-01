package net.alasc.symdpoly

import shapeless.HMap

/** This package provides pretty printing methods for the objects in symdpoly. */
package object pretty {
  /** List of settings, that is a typesafe map containing pairs of keys and their values. */
  type Settings = HMap[Key.Relation]
  /** Whether to add whitespace to align columns of pretty printed matrices. */
  val matAlignColumns: Key.Aux[Boolean] = Key(true)
  object syntax extends PrettySyntax
}
