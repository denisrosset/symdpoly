package net.alasc.symdpoly

import scala.collection.immutable.SortedSet
import shapeless.Witness
import spire.algebra.Action
import spire.syntax.action._
import net.alasc.finite.Grp
import net.alasc.symdpoly
import net.alasc.symdpoly.freebased.{Mono, MonoidDef}
import net.alasc.symdpoly.util.OrderedSet
import syntax.phased._

/** Generating set of monomials.
  *
  * Compared to a standard Scala Set, it preserves the structure of its construction, and provides additional
  * combinators.
  *
  */
sealed trait GSet[M <: generic.MonoidDef with Singleton] { lhs =>

  /** Computes and returns the sorted generating set of monomials. */
  def toSortedSet(implicit wM: Witness.Aux[M]): SortedSet[M#Monomial]

  /** Computes and returns the sorted generating set of monomials as an OrderedSet. */
  def toOrderedSet(implicit wM: Witness.Aux[M]): OrderedSet[M#Monomial] = OrderedSet.fromSortedSet(toSortedSet)

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

}

object GSet {

  /** Additional operations for generating sets on free-based monomials. */
  implicit class RichGSet[
    M <: MonoidDef.Aux[F] with Singleton,
    F <: free.MonoidDef.Aux[F] with Singleton
  ](val lhs: GSet[M with MonoidDef.Aux[F]]) {

    /** Completes a generating set of monomials by their orbit under a group. */
    def orbit[G](grp: Grp[G])(implicit action: Action[Mono[M, F], G]): GSet[M] = Orbit[M, G](lhs, grp)

  }

  /** Ordering typeclass for Scala collections. */
  def ordering[M <: generic.MonoidDef with Singleton](implicit witness: Witness.Aux[M]): Ordering[M#Monomial] =
    spire.compat.ordering((witness.value: M).monoOrder)

  case class Quotient[M <: MonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](preimage: GSet[F]) extends GSet[M] {
    override def toString: String = s"Quotient($preimage)"
    def toSortedSet(implicit wM: Witness.Aux[M]): SortedSet[Mono[M, F]] = {
      def M: M = wM.value
      implicit def wF: Witness.Aux[F] = (M.Free: F).witness
      implicit val o: Ordering[Mono[M, F]] = ordering[M]
        preimage.toSortedSet.map(mono => M.quotient(mono))
    }
  }

  case class Sequence[M <: generic.MonoidDef with Singleton](seq: Seq[GSet[M]]) extends GSet[M] {
    override def toString: String = seq.mkString("[",",","]")
    def toSortedSet(implicit wM: Witness.Aux[M]): SortedSet[M#Monomial] = {
      implicit def o: Ordering[M#Monomial] = ordering[M]
      seq.foldLeft(SortedSet.empty[M#Monomial])((set, gm) => set union gm.toSortedSet )
    }
  }

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

  protected case class Orbit[
    M <: generic.MonoidDef with Singleton,
    G
  ](gm: GSet[M], grp: Grp[G])(implicit action: Action[M#Monomial, G]) extends GSet[M] {
    override def toString: String = s"Orbit($gm)"
    def toSortedSet(implicit wM: Witness.Aux[M]): SortedSet[M#Monomial] = {
      implicit def o: Ordering[M#Monomial] = spire.compat.ordering((wM.value: M).monoOrder)
      for {
        m <- gm.toSortedSet
        g <- grp.iterator
      } yield valueOf[M].monoPhased.phaseCanonical(m <|+| g)
    }
  }

  protected case class Empty[M <: generic.MonoidDef with Singleton]() extends GSet[M] {
    override def toString: String = "{}"
    def toSortedSet(implicit wM: Witness.Aux[M]): SortedSet[M#Monomial] = {
      implicit def o: Ordering[M#Monomial] = ordering[M]
      SortedSet.empty[M#Monomial]
    }
  }

  protected case class Id[M <: generic.MonoidDef with Singleton]() extends GSet[M] {
    override def toString: String = "1"
    def toSortedSet(implicit wM: Witness.Aux[M]): SortedSet[M#Monomial] = {
      implicit def o: Ordering[M#Monomial] = ordering[M]
      SortedSet((wM.value: M).one)
    }
  }

  protected case class Ops[F <: free.MonoidDef.Aux[F] with Singleton](opEnum: F#OpEnum) extends GSet[F] {
    override def toString: String =
      if (opEnum.allInstances.isEmpty) opEnum.toString
      else opEnum.allInstances.head.productPrefix
    def toSortedSet(implicit wF: Witness.Aux[F]): SortedSet[F#Monomial] = {
      implicit def o: Ordering[F#Monomial] = ordering[F]
      opEnum.allInstances.map(op => freebased.Mono.fromOp(op): F#Monomial).to[SortedSet]
    }
  }

  protected case class Word[F <: free.MonoidDef.Aux[F] with Singleton](seq: Seq[F#OpEnum]) extends GSet[F] {
    def opEnumString(opEnum: F#OpEnum): String =
      if (opEnum.allInstances.isEmpty) opEnum.toString
      else opEnum.allInstances.head.productPrefix

    override def toString: String = seq.map(opEnumString).mkString("*")

    def toSortedSet(implicit wF: Witness.Aux[F]): SortedSet[F#Monomial] = {
      def F: F = wF.value
      implicit def o: Ordering[F#Monomial] = ordering[F]
      seq match {
        case Seq() => SortedSet(F.one: F#Monomial)
        case Seq(op) => op.allInstances.map(op => freebased.Mono.fromOp(op): F#Monomial).to[SortedSet]
        case Seq(hd, tl@_*) => for {
          x <- hd.allInstances.map(op => freebased.Mono.fromOp(op)).to[SortedSet]
          y <- Word(tl).toSortedSet
        } yield F.monoMultiplicativeBinoid.times(x, y)
      }
    }
  }

  protected case class Power[M <: generic.MonoidDef with Singleton](gm: GSet[M], exp: Int) extends GSet[M] {
    override def toString: String = s"($gm)^$exp"
    def toSortedSet(implicit wM: Witness.Aux[M]): SortedSet[M#Monomial] = {
      def M: M = wM.value
      implicit def o: Ordering[M#Monomial] = ordering[M]
      exp match {
        case 0 => SortedSet((wM.value: M).one)
        case 1 => gm.toSortedSet
        case k =>
          for {
            x <- gm.toSortedSet
            y <- Power(gm, exp - 1).toSortedSet
          } yield M.monoMultiplicativeBinoid.times(x, y)
      }
    }
  }

  protected case class Tensor[M <: generic.MonoidDef with Singleton](seq: Seq[GSet[M]]) extends GSet[M] {
    override def toString: String = seq.mkString("*")
    def toSortedSet(implicit wM: Witness.Aux[M]): SortedSet[M#Monomial] = {
      def M: M = wM.value
      implicit def o: Ordering[M#Monomial] = ordering[M]
      seq match {
        case Seq() => SortedSet(M.one)
        case Seq(gm) => gm.toSortedSet
        case Seq(hd, tl @ _*) =>
          for {
            x <- hd.toSortedSet
            y <- Tensor(tl).toSortedSet
          } yield M.monoMultiplicativeBinoid.times(x, y)
      }
    }
  }

}
