package net.alasc.symdpoly
package free

class FreeMono[F <: MonoidDef with Singleton](val data: MutableWord[F]) extends generic.GenMono[F, FreeMono[F]] {
  def length: Int = data.length
  def apply(i: Int): F#Op = data(i)
  def mutableCopy: MutableWord[F] = data.mutableCopy
}
