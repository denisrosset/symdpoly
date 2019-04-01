package net.alasc.symdpoly.pretty

import shapeless.HMap
import syntax._

/** An output format for pretty printing. */
trait Format { self =>
  type Output
  def settings: HMap[Key.Relation] = HMap.empty[Key.Relation]
  def apply(key: Key): key.V = settings.value(key)
}

object Format {
  type Aux[O] = Format { type Output = O }
}
