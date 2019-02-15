package net.alasc.symdpoly.pretty

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import shapeless.HMap

import net.alasc.symdpoly.pretty.PrettySyntax.{PrettyOps, SettingsOps}
import syntax._

/** Syntax for pretty printing objects */
trait PrettySyntax {
  implicit def prettyOps[A](a: A): PrettyOps[A] = new PrettyOps[A](a)
  implicit def settingsOps(settings: Settings): SettingsOps = new SettingsOps(settings)
}

object PrettySyntax {

  /** Rich operations for the settings map */
  class SettingsOps(val settings: Settings) extends AnyVal {
    def ++(additionalSettings: Seq[Setting]): HMap[Key.Relation] =
      additionalSettings.foldLeft(settings) {
        case (hm, setting) => hm + ( setting.key -> setting.value )
      }
    def value(key: Key): key.V = settings.get[key.type, key.V](key).getOrElse(key.defaultValue)
  }

  /** Syntax for pretty printing operations. */
  class PrettyOps[A](val a: A) {
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

}