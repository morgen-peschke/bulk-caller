package peschke.bulk_calls
package models

import cats.syntax.apply._
import cats.syntax.validated._
import com.monovore.decline.Argument

final case class KeyValueArgument[K,V](key: K, value: V) {
  def tupled: (K,V) = key -> value
}
object KeyValueArgument {
  implicit final def argument[K,V](implicit K: Argument[K], V: Argument[V]): Argument[KeyValueArgument[K, V]] = {
    val defaultMetaVar = s"${K.defaultMetavar}=${V.defaultMetavar}"
    Argument.from(defaultMetaVar) {
      _.split('=').toList match {
        case key :: rest => (K.read(key), V.read(rest.mkString("="))).mapN(KeyValueArgument[K, V])
        case Nil => s"Expected $defaultMetaVar".invalidNel
      }
    }
  }
}
