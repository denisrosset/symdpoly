package net.alasc.symdpoly

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.reflect.ClassTag

import shapeless.Witness
import spire.algebra.{Action, Monoid, Rig}
import spire.syntax.action._
import spire.syntax.monoid._
import spire.syntax.ring._

import net.alasc.finite.Grp
import net.alasc.symdpoly.freebased.{Mono, MonoidDef}
import net.alasc.symdpoly.util.OrderedSet
import syntax.phased._
import metal.immutable.{HashSet => MetalSet}
import metal.syntax._

/** Generating set of monomials.
  *
  * Compared to a standard Scala Set, it preserves the structure of its construction, and provides additional
  * combinators.
  *
  */
sealed trait GSet[M <: generic.MonoidDef with Singleton] { lhs =>

  /** Computes and returns the sorted generating set of monomials. */
  def metalSet(implicit wM: Witness.Aux[M]): MetalSet[M#Monomial]

  /** Computes and returns the sorted generating set of monomials as an OrderedSet. */
  def toOrderedSet(implicit wM: Witness.Aux[M]): OrderedSet[M#Monomial] =
    OrderedSet.fromUnique(metalSet.toScala)

  /** Union. */
  def +(rhs: GSet[M]): GSet[M] =
    lhs match {
      case GSet.Sequence(seq) => GSet.Sequence(seq :+ rhs)
      case _ => GSet.Sequence(Seq(lhs, rhs))
    }

  /** Cartesian product. */
  def *(rhs: GSet[M]): GSet[M] =
    lhs match {
      case GSet.Tensor(seq) => GSet.Tensor(seq :+ rhs)
      case _ => GSet.Tensor(Seq(lhs, rhs))
    }

  /** Power. */
  def pow(exp: Int): GSet[M] = GSet.Power(lhs, exp)

  /** Completes a generating set of monomials by their orbit under a group. */
  def orbit(grp: Grp[M#Permutation])(implicit action: Action[M#Monomial, M#Permutation]): GSet[M] = GSet.Orbit[M](lhs, grp)

}

object GSet {

  /** Construct a set of monomials of degree 1 containing the given operators. */
  def apply[F <: free.MonoidDef.Aux[F] with Singleton](opEnums: F#OpEnum*): GSet[F] =
    if (opEnums.length == 0) empty[F]
    else if (opEnums.length == 1) Ops(opEnums(0))
    else Sequence(opEnums.map(Ops(_)))

  /** Construct a set of monomials of degree 1 containing the given operators, adjoined with the identity. */
  def onePlus[F <: free.MonoidDef.Aux[F] with Singleton](opEnums: F#OpEnum*): GSet[F] =
    if (opEnums.length == 0) id[F]
    else Sequence(id[F] +: opEnums.map(Ops(_)))

  /** Construct all possible words containing the given operator types. */
  def word[F <: free.MonoidDef.Aux[F] with Singleton](opEnums: F#OpEnum*): GSet[F] =
    if (opEnums.length == 0) id[F]
    else Word(opEnums)

  /** Empty generating set of monomials. */
  def empty[M <: generic.MonoidDef with Singleton]: GSet[M] = Empty[M]

  /** Generating set of monomials containing the identity only. */
  def id[M <: generic.MonoidDef with Singleton]: GSet[M] = Id[M]()

  protected case class Orbit[M <: generic.MonoidDef with Singleton](gm: GSet[M], grp: Grp[M#Permutation]) extends GSet[M] {
    override def toString: String = s"Orbit($gm)"
    def metalSet(implicit wM: Witness.Aux[M]): MetalSet[M#Monomial] = {
      import generic.MonoidDef.permutationMonoAction
      val res = metal.mutable.HashSet.empty[M#Monomial]
      val on = gm.metalSet
      grp.iterator.foreach { g =>
        on.foreach { m =>
          res += (m <|+| g).phaseCanonical
        }
      }
      res.result()
    }
  }

  protected case class Empty[M <: generic.MonoidDef with Singleton]() extends GSet[M] {
    override def toString: String = "{}"
    def metalSet(implicit wM: Witness.Aux[M]): MetalSet[M#Monomial] = metal.immutable.HashSet.empty
  }

  protected case class Id[M <: generic.MonoidDef with Singleton]() extends GSet[M] {
    override def toString: String = "1"
    def metalSet(implicit wM: Witness.Aux[M]): MetalSet[M#Monomial] = metal.immutable.HashSet(valueOf[M].one)
  }

  protected case class Ops[F <: free.MonoidDef.Aux[F] with Singleton](opEnum: F#OpEnum) extends GSet[F] {
    override def toString: String =
      if (opEnum.allInstances.isEmpty) opEnum.toString
      else opEnum.allInstances.head.productPrefix
    def metalSet(implicit wF: Witness.Aux[F]): MetalSet[F#Monomial] = {
      val res = metal.mutable.HashSet.empty[F#Monomial]
      opEnum.allInstances.foreach { op =>
        res += op.toMono
      }
      res.result()
    }
  }

  protected case class Word[F <: free.MonoidDef.Aux[F] with Singleton](seq: Seq[F#OpEnum]) extends GSet[F] {
    def opEnumString(opEnum: F#OpEnum): String =
      if (opEnum.allInstances.isEmpty) opEnum.toString
      else opEnum.allInstances.head.productPrefix
    override def toString: String = seq.map(opEnumString).mkString("*")
    def metalSet(implicit wF: Witness.Aux[F]): MetalSet[F#Monomial] = Tensor[F](seq.map(Ops(_))).metalSet
  }

  case class Quotient[M <: MonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](preimage: GSet[F]) extends GSet[M] {
    override def toString: String = s"Quotient($preimage)"
    def metalSet(implicit wM: Witness.Aux[M]): MetalSet[Mono[M, F]] = preimage match {
      case Sequence(seq) => Sequence(seq.map(Quotient[M, F](_))).metalSet
      case Tensor(seq) => Tensor(seq.map(Quotient[M, F](_))).metalSet
      case Power(gm, exp) => Power(Quotient[M, F](gm), exp).metalSet
      case _ =>
      import generic.MonoidDef.monoPhased
      def M: M = wM.value
      implicit def wF: Witness.Aux[F] = (M.Free: F).witness
      val inF = preimage.metalSet
      val res = metal.mutable.HashSet.reservedSize[M#Monomial](inF.size)
      inF.foreach { monoF => res += M.quotient(monoF).phaseCanonical }
      res.result()
    }
  }

  case class Sequence[M <: generic.MonoidDef with Singleton](seq: Seq[GSet[M]]) extends GSet[M] {
    override def toString: String = seq.mkString("[",",","]")
    def metalSet(implicit wM: Witness.Aux[M]): MetalSet[M#Monomial] =
      monoidSetSemiring[M#Monomial](valueOf[M].monoClassTag, canonicalMonoMonoid[M]).sum(seq.map(_.metalSet))
  }


  protected case class Power[M <: generic.MonoidDef with Singleton](gm: GSet[M], exp: Int) extends GSet[M] {
    override def toString: String = s"($gm)^$exp"
    def metalSet(implicit wM: Witness.Aux[M]): MetalSet[M#Monomial] =
      monoidSetSemiring[M#Monomial](valueOf[M].monoClassTag, canonicalMonoMonoid[M]).pow(gm.metalSet, exp)
  }

  protected case class Tensor[M <: generic.MonoidDef with Singleton](seq: Seq[GSet[M]]) extends GSet[M] {
    override def toString: String = seq.mkString("*")
    def metalSet(implicit wM: Witness.Aux[M]): MetalSet[M#Monomial] =
      monoidSetSemiring[M#Monomial](valueOf[M].monoClassTag, canonicalMonoMonoid[M]).product(seq.map(_.metalSet))
  }

  protected def canonicalMonoMonoid[M <: generic.MonoidDef with Singleton: Witness.Aux]: Monoid[M#Monomial] = new Monoid[M#Monomial] {
    def empty: M#Monomial = valueOf[M].one
    def combine(x: M#Monomial, y: M#Monomial): M#Monomial = {
      import generic.MonoidDef.{monoMultiplicativeBinoid, monoPhased}
      (x * y).phaseCanonical
    }
  }

  protected implicit def monoidSetSemiring[A:ClassTag:Monoid]: Rig[MetalSet[A]] = new Rig[MetalSet[A]] {
    def zero: MetalSet[A] = metal.immutable.HashSet.empty[A]
    def plus(x: MetalSet[A], y: MetalSet[A]): MetalSet[A] = {
      val res = x.mutableCopy
      y.foreach { m => res += m }
      res.result()
    }
    def one: MetalSet[A] = metal.immutable.HashSet(Monoid[A].empty)
    def times(x: MetalSet[A], y: MetalSet[A]): MetalSet[A] = {
      val res = metal.mutable.HashSet.empty[A]
      x.foreach { m1 =>
        y.foreach { m2 =>
          res += (m1 |+| m2)
        }
      }
      res.result()
    }

    override def pow(a: MetalSet[A], n: Int): MetalSet[A] = super.pow(a, n)

    override def product(as: TraversableOnce[MetalSet[A]]): MetalSet[A] = as match {
      case iterable: Iterable[MetalSet[A]] =>
        val it = iterable.iterator
        if (!it.hasNext) one else {
          val first = it.next()
          if (!it.hasNext) first else {
            def rec(prev: metal.mutable.HashSet[A], next: metal.mutable.HashSet[A]): MetalSet[A] =
              if (!it.hasNext) prev.result() else {
                val rhs = it.next()
                next.reset()
                prev.foreach { m1 =>
                  rhs.foreach { m2 =>
                    next += (m1 |+| m2)
                  }
                }
                rec(next, prev)
              }

            rec(first.mutableCopy, metal.mutable.HashSet.empty[A])
          }
        }
      case _=> super.product(as)
    }
  }

}
