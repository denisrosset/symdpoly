package net.alasc.symdpoly.pretty

/** Key for a pretty printing setting */
trait Key { self =>
  /** Value type of this key */
  type V
  def name: String
  def defaultValue: V
  /** Sets the value of this key, returing a setting */
  def :=(value: V): Setting.Aux[self.type] = Setting(self, value)
}

object Key {
  type Aux[V0] = Key { type V = V0 }
  /** Factory method to create a key with a default value. The name is extracted from the lhs assignment. */
  def apply[V0](defaultValue0: V0)(implicit name0: sourcecode.Name): Key.Aux[V0] = new Key {
    type V = V0
    def name: String = name0.value
    def defaultValue: V = defaultValue0
  }
  /** Relation between a key and its value, used to construct a `shapeless.HMap` */
  class Relation[K, V]
  object Relation {
    implicit def derivation[K <: Key with Singleton]: Relation[K, K#V] = new Relation[K, K#V]
  }
}
