package net.alasc.symdpoly.pretty

import shapeless.HMap

import scalin.immutable.Mat

/** Matlab source code format. */
object MatlabSource extends Format {
  type Output = String
  implicit val int: Pretty[Int, MatlabSource.type] = Pretty.noSettings[Int, MatlabSource.type](_.toString)
  implicit def mat[A](implicit ev: Pretty[A, MatlabSource.type]): Pretty[Mat[A], MatlabSource.type] =
    new Pretty[Mat[A], MatlabSource.type] {
      def apply(mat: Mat[A])(implicit settings: HMap[Key.Relation]): String =
        Seq.tabulate(mat.nRows) {
          r => Seq.tabulate(mat.nCols)(c => ev(mat(r, c))).mkString(",")
        }.mkString("[", "\n", "]")
    }
}
