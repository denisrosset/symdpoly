package net.alasc.symdpoly

import spire.util.Opt

/** A set of default imports, syntax extensions and instances to work easily with SymDPoly. */
object defaults
    extends net.alasc.perms.Algorithms(Opt(scala.util.Random), false, true)
    with spire.syntax.AllSyntax
    with spire.std.AnyInstances
    with net.alasc.syntax.AllSyntax
    with net.alasc.std.AnyInstances
    with scalin.syntax.AllSyntax
    with instances.AllInstances
    with syntax.AllSyntax
{

  implicit def vecEngine[A]: scalin.VecEngine[A, scalin.immutable.DenseVec[A]] = scalin.immutable.DenseVec.defaultEngine[A]

  implicit def matEngine[A]: scalin.MatEngine[A, scalin.immutable.DenseMat[A]] = scalin.immutable.DenseMat.defaultEngine[A]

  type Vec[A] = scalin.immutable.Vec[A]
  val Vec = scalin.immutable.Vec

  type Mat[A] = scalin.immutable.Mat[A]
  val Mat = scalin.immutable.Mat

  type Rational = spire.math.Rational
  val Rational = spire.math.Rational

  type SafeLong = spire.math.SafeLong
  val SafeLong = spire.math.SafeLong

  type Grp[G] = net.alasc.finite.Grp[G]
  val Grp = net.alasc.finite.Grp

  type Perm = net.alasc.perms.Perm
  val Perm = net.alasc.perms.Perm

  type Cyclo = cyclo.Cyclo
  val Cyclo = cyclo.Cyclo

  def cosRev(n: Int, d: Int): Cyclo = Cyclo.cosRev(Rational(n, d))

  def sinRev(n: Int, d: Int): Cyclo = Cyclo.sinRev(Rational(n, d))

  def sqrt(n: Int): Cyclo = Cyclo.sqrt(n)

  def sqrt(r: Rational): Cyclo = Cyclo.sqrt(r)

  def e(n: Int): Cyclo = Cyclo.e(n)

}
