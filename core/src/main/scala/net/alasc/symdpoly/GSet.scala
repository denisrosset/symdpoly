package net.alasc.symdpoly

import scala.collection.immutable.SortedSet

import shapeless.Witness
import spire.algebra.Action
import spire.syntax.action._

import net.alasc.finite.Grp
import net.alasc.symdpoly
import net.alasc.symdpoly.generic.FreeBasedMonoidDef

/** Generating set of monomials. */
sealed trait GSet[M <: generic.MonoidDef with Singleton] { lhs =>
  def monomials(implicit wM: Witness.Aux[M]): SortedSet[M#Monomial]
  def +(rhs: GSet[M]): GSet[M] =
    lhs match {
      case GSet.Sequence(seq) => GSet.Sequence(seq :+ rhs)
      case _ => GSet.Sequence(Seq(lhs, rhs))
    }
  def *(rhs: GSet[M]): GSet[M] =
    lhs match {
      case GSet.Tensor(seq) => GSet.Tensor(seq :+ rhs)
      case _ => GSet.Tensor(Seq(lhs, rhs))
    }
  def pow(exp: Int): GSet[M] = GSet.Power(lhs, exp)
}

object GSet {

  implicit class RichGSet[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](val lhs: GSet[M]) {
    def orbit[G](grp: Grp[G])(implicit action: Action[Mono[M, F], G]): GSet[M] = Orbit[G, M, F](lhs, grp)
  }

  def ordering[M <: generic.MonoidDef with Singleton](implicit witness: Witness.Aux[M]): Ordering[M#Monomial] =
    spire.compat.ordering((witness.value: M).monoOrder)

  case class Quotient[M <: FreeBasedMonoidDef.Aux[F] with Singleton, F <: free.MonoidDef.Aux[F] with Singleton](preimage: GSet[F]) extends GSet[M] {
    override def toString: String = s"Quotient($preimage)"
    def monomials(implicit wM: Witness.Aux[M]): SortedSet[Mono[M, F]] = {
      def M: M = wM.value
      implicit def wF: Witness.Aux[F] = (M.Free: F).witness
      implicit val o: Ordering[Mono[M, F]] = ordering[M]
      preimage.monomials.map(mono => M.quotient(mono))
    }
  }

  case class Sequence[M <: generic.MonoidDef with Singleton](seq: Seq[GSet[M]]) extends GSet[M] {
    override def toString: String = seq.mkString("[",",","]")
    def monomials(implicit wM: Witness.Aux[M]): SortedSet[M#Monomial] = {
      implicit def o: Ordering[M#Monomial] = ordering[M]
      seq.foldLeft(SortedSet.empty[M#Monomial])((set, gm) => set ++ gm.monomials )
    }
  }

  /** Construct a set of monomials of degree 1 containing the given operators. */
  def apply[F <: free.MonoidDef.Aux[F] with Singleton](opTypes: F#OpType*): GSet[F] =
    if (opTypes.length == 0) empty[F]
    else if (opTypes.length == 1) Ops(opTypes(0))
    else Sequence(opTypes.map(Ops(_)))

  /** Construct a set of monomials of degree 1 containing the given operators, adjoined with the identity. */
  def onePlus[F <: free.MonoidDef.Aux[F] with Singleton](opTypes: F#OpType*): GSet[F] =
    if (opTypes.length == 0) id[F]
    else Sequence(id[F] +: opTypes.map(Ops(_)))

  /** Construct all possible words containing the given operator types. */
  def word[F <: free.MonoidDef.Aux[F] with Singleton](opTypes: F#OpType*): GSet[F] =
    if (opTypes.length == 0) id[F]
    else Word(opTypes)

  def empty[M <: generic.MonoidDef with Singleton]: GSet[M] = Empty[M]

  def id[M <: generic.MonoidDef with Singleton]: GSet[M] = Id[M]()

  case class Orbit[G, M <: generic.FreeBasedMonoidDef.Aux[F] with Singleton,
  F <: free.MonoidDef.Aux[F] with Singleton](gm: GSet[M], grp: Grp[G])
                                            (implicit action: Action[Mono[M, F], G]) extends GSet[M] {
    override def toString: String = s"Orbit($gm)"
    def monomials(implicit wM: Witness.Aux[M]): SortedSet[Mono[M, F]] = {
      implicit def o: Ordering[Mono[M, F]] = spire.compat.ordering((wM.value: M).monoOrder)
      for {
        m <- gm.monomials
        g <- grp.iterator
      } yield (m <|+| g).phaseCanonical
    }
  }

  case class Empty[M <: generic.MonoidDef with Singleton]() extends GSet[M] {
    override def toString: String = "{}"
    def monomials(implicit wM: Witness.Aux[M]): SortedSet[M#Monomial] = {
      implicit def o: Ordering[M#Monomial] = ordering[M]
      SortedSet.empty[M#Monomial]
    }
  }
  case class Id[M <: generic.MonoidDef with Singleton]() extends GSet[M] {
    override def toString: String = "1"
    def monomials(implicit wM: Witness.Aux[M]): SortedSet[M#Monomial] = {
      implicit def o: Ordering[M#Monomial] = ordering[M]
      SortedSet((wM.value: M).one)
    }
  }
  case class Ops[F <: free.MonoidDef.Aux[F] with Singleton](opType: F#OpType) extends GSet[F] {
    override def toString: String = opType.toString // TODO or allInstances.head.productPrefix
    def monomials(implicit wF: Witness.Aux[F]): SortedSet[F#Monomial] = {
      implicit def o: Ordering[F#Monomial] = ordering[F]
      opType.allInstances.map(op => symdpoly.Mono.fromOp(op): F#Monomial).to[SortedSet]
    }
  }

  case class Word[F <: free.MonoidDef.Aux[F] with Singleton](seq: Seq[F#OpType]) extends GSet[F] {
    override def toString: String = seq.map(_.toString).mkString("*")
    def monomials(implicit wF: Witness.Aux[F]): SortedSet[F#Monomial] = {
      def F: F = wF.value
      implicit def o: Ordering[F#Monomial] = ordering[F]
      seq match {
        case Seq() => SortedSet(F.one: F#Monomial)
        case Seq(op) => op.allInstances.map(op => symdpoly.Mono.fromOp(op): F#Monomial).to[SortedSet]
        case Seq(hd, tl@_*) => for {
          x <- hd.allInstances.map(op => symdpoly.Mono.fromOp(op)).to[SortedSet]
          y <- Word(tl).monomials
        } yield F.monoMultiplicativeMonoid.times(x, y)
      }
    }
  }

  case class Power[M <: generic.MonoidDef with Singleton](gm: GSet[M], exp: Int) extends GSet[M] {
    override def toString: String = s"($gm)^$exp"
    def monomials(implicit wM: Witness.Aux[M]): SortedSet[M#Monomial] = {
      def M: M = wM.value
      implicit def o: Ordering[M#Monomial] = ordering[M]
      exp match {
        case 0 => SortedSet((wM.value: M).one)
        case 1 => gm.monomials
        case k =>
          for {
            x <- gm.monomials
            y <- Power(gm, exp - 1).monomials
          } yield M.monoMultiplicativeMonoid.times(x, y)
      }
    }
  }

  case class Tensor[M <: generic.MonoidDef with Singleton](seq: Seq[GSet[M]]) extends GSet[M] {
    override def toString: String = seq.mkString("*")
    def monomials(implicit wM: Witness.Aux[M]): SortedSet[M#Monomial] = {
      def M: M = wM.value
      implicit def o: Ordering[M#Monomial] = ordering[M]
      seq match {
        case Seq() => SortedSet(M.one)
        case Seq(gm) => gm.monomials
        case Seq(hd, tl @ _*) =>
          for {
            x <- hd.monomials
            y <- Tensor(tl).monomials
          } yield M.monoMultiplicativeMonoid.times(x, y)
      }
    }
  }

}
