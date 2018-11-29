package net.alasc.symdpoly

import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.perms.Perm
import net.alasc.symdpoly.free.Generator
import net.alasc.symdpoly.generic.FreeBasedMonoidDef
import net.alasc.symdpoly.math.{GenPerm, Phases}
import shapeless.Witness
import spire.syntax.cfor.cforRange
import net.alasc.perms.default._

abstract class AbstractGroup {
  type El
  def grp: Grp[El]
  implicit def fpab: FaithfulPermutationActionBuilder[El]
}

abstract class AmbientGroup[
  M <: FreeBasedMonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton
](implicit wM: Witness.Aux[M]) extends AbstractGroup {
  def M: M = wM.value
  val F: F = M.Free


  type El = GenPerm
  def fpab: FaithfulPermutationActionBuilder[El] = GenPerm.fpab
  def generator(f: F#Op => F#PhasedOp)(implicit name: sourcecode.Name): El = {
    import scala.collection.mutable.{HashMap => MMap}
    val phaseMap: MMap[Int, Phase] = MMap.empty[Int, Phase]
    val permImages = new Array[Int](F.nOperators)
    cforRange(0 until F.nOperators) { i =>
      val F.PhasedOp(newPhase, newOp) = f(F.opFromIndex(i))
      val newI = F.indexFromOp(newOp)
      phaseMap(newI) = newPhase
      permImages(i) = newI
    }
    val perm = Perm.fromImages(permImages)
    val phases = Phases(phaseMap.toVector: _*)
    GenPerm(perm, phases)
  }

  def generators: Seq[El]

  lazy val grp: Grp[El] = Grp.fromGenerators(generators)
}
