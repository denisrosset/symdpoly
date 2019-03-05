package net.alasc.symdpoly.pretty

/** Pretty printing setting associating a key with a value */
trait Setting {
  /** Key type of which this provides a value */
  type K <: Key with Singleton
  /** Key instance */
  def key: K
  /** Value */
  def value: K#V
}

object Setting {
  type Aux[K0 <: Key with Singleton] = Setting { type K = K0 }
  /** Constructs as Setting instance */
  def apply[K0 <: Key with Singleton](key0: K0, value0: K0#V): Setting.Aux[K0] = new Setting {
    type K = K0
    def key: K = key0
    def value: K#V = value0
  }
}
