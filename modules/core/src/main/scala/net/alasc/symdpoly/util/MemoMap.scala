package net.alasc.symdpoly.util

/** Map that caches evaluations of the function "f".
  *
  * Previous evaluations are stored in a WeakHashMap, whose content can be reclaimed by the garbage collector.
  */
class MemoMap[K, V](f: K => V) {
  override def toString: String = s"MemoMap with ${map.size} entries"
  private[this] val map = new java.util.WeakHashMap[K, V]
  def apply(key: K): V = map.get(key) match {
    case null =>
      val value = f(key)
      map.put(key, value)
      value
    case value => value
  }
}

object MemoMap {

  def apply[K, V](f: K => V): MemoMap[K, V] = new MemoMap[K, V](f)

}
