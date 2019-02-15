package net.alasc.symdpoly
package pretty

import java.io.File
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import scala.annotation.tailrec

import shapeless.{HMap, Witness}

import cyclo.Cyclo
import scalin.immutable.Mat
import spire.syntax.cfor._

import syntax._

object instances {
  val matAlignColumns: Key.Aux[Boolean] = Key(true)
  implicit def mat[A](implicit ev: Pretty[A, Text.type]): Pretty[Mat[A], Text.type] =
    new Pretty[Mat[A], Text.type] {
      def apply(mat: Mat[A])(implicit settings: HMap[Key.Relation]): String =
        if (settings.value(matAlignColumns)) {
          @tailrec def colWidth(c: Int, r: Int = 0, width: Int = 0): Int =
            if (r >= mat.nRows) width else colWidth(c, r + 1, spire.math.max(width, ev(mat(r, c)).length + 2))
          val colWidths = Array.tabulate(mat.nCols)(c => colWidth(c))
          val rv = new scala.StringBuilder
          cforRange(0 until mat.nRows) { r =>
            cforRange(0 until mat.nCols) { c =>
              val cell = ev(mat(r, c))
              rv.append(" " * (colWidths(c) - cell.length))
              rv.append(cell)
            }
            if (r + 1 < mat.nRows) rv.append("\n")
          }
          rv.toString
        } else
          Seq.tabulate(mat.nRows)(r => Seq.tabulate(mat.nCols)(c => ev(mat(r, c))).mkString(" ")).mkString("\n")
    }
}

/** An output format for pretty printing. */
trait Format { self =>
  type Output
  def settings: HMap[Key.Relation] = HMap.empty[Key.Relation]
  def apply(key: Key): key.V = settings.value(key)
}

object Format {
  type Aux[O] = Format { type Output = O }
}

/** Text format, for example to output on the console, or to write into text files. */
object Text extends Format {
  type Output = String
  implicit val cyclo: Pretty[Cyclo, Text.type] = Pretty.noSettings[Cyclo, Text.type](_.toString)
  implicit val int: Pretty[Int, Text.type] = Pretty.noSettings[Int, Text.type](_.toString)
}

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

/** Describes the ability to pretty print instances of A in the format F. */
trait Pretty[A, F <: Format with Singleton] {
  def apply(a: A)(implicit settings: HMap[Key.Relation]): F#Output
}

object syntax {
  /** Syntax for pretty printing operations. */
  implicit class PrettyOps[A](val a: A) {
    /** Writes a pretty printed description to file
      *
      * @param filename           Name of the file to write
      * @param format             Textual format to use
      * @param additionalSettings Additional pretty printing settings
      */
    def prettyWrite[F <: Format.Aux[String] with Singleton](filename: String, format: F, additionalSettings: Setting*)(implicit ev: Pretty[A, F]): Unit =
      Files.write(Paths.get(filename), prettyFormat(format: F, additionalSettings: _*).getBytes(StandardCharsets.UTF_8))
    /** Writes a pretty printed description to file using the default [[Text]] format.
      *
      * @param filename           Name of the file to write
      * @param additionalSettings Additional pretty printing settings
      */
    def prettyWrite(filename: String, additionalSettings: Setting*)(implicit ev: Pretty[A, Text.type]): Unit =
      Files.write(Paths.get(filename), prettyFormat(Text, additionalSettings: _*).getBytes(StandardCharsets.UTF_8))

    /** Pretty prints to the standard output
      *
      * @param format             Textual format to use
      * @param additionalSettings Additional settings
      */
    def prettyPrint[F <: Format.Aux[String] with Singleton](format: F, additionalSettings: Setting*)(implicit ev: Pretty[A, F]): Unit =
      println(prettyFormat(format: F, additionalSettings: _*))
    /** Pretty prints to the standard output using the default [[Text]] format.
      *
      * @param additionalSettings Additional settings
      */
    def prettyPrint(additionalSettings: Setting*)(implicit ev: Pretty[A, Text.type]): Unit =
      println(prettyFormat(Text, additionalSettings: _*))
    /** Formats the object using the given format.
      *
      * @param format             Format to use
      * @param additionalSettings Additional settings
      */
    def prettyFormat[F <: Format with Singleton](format: F, additionalSettings: Setting*)(implicit ev: Pretty[A, F]): F#Output =
      if (additionalSettings.isEmpty) ev(a)(format.settings) else ev(a)(format.settings ++ additionalSettings)

    /** Formats the object in the format F, using the settings passed as an implicit parameter. */
    def pretty[F <: Format with Singleton](implicit ev: Pretty[A, F], settings: HMap[Key.Relation]): F#Output = ev(a)
  }
  /** Rich operations for the settings map */
  implicit class SettingsHMapOps(val settings: HMap[Key.Relation]) extends AnyVal {
    def ++(additionalSettings: Seq[Setting]): HMap[Key.Relation] =
      additionalSettings.foldLeft(settings) {
        case (hm, setting) => hm + ( setting.key -> setting.value )
      }
    def value(key: Key): key.V = settings.get[key.type, key.V](key).getOrElse(key.defaultValue)
  }
}

object Pretty {
  /** Returns an implicit instance of Pretty. */
  def apply[A, F <: Format with Singleton](implicit ev: Pretty[A, F]): Pretty[A, F] = ev
  /** Helper factory method to construct a Pretty instance that does not use settings. */
  def noSettings[A, F <: Format with Singleton](f: A => F#Output): Pretty[A, F] = new Pretty[A, F] {
    def apply(a: A)(implicit settings: HMap[Key.Relation]): F#Output = f(a)
  }
}

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
  /** Relation between a key and its value, used to construct a [[shapeless.HMap]] */
  class Relation[K, V]
  object Relation {
    implicit def derivation[K <: Key with Singleton]: Relation[K, K#V] = new Relation[K, K#V]
  }
}
