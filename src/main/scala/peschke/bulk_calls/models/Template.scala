package peschke.bulk_calls
package models

import models.Template.Element
import utils.NewType

import cats.Show
import cats.data.NonEmptyList
import io.circe.generic.semiauto._
import io.circe.{Codec, KeyDecoder}

final case class Template(elements: NonEmptyList[Element])
object Template {

  object Name extends NewType[String] {
    implicit final val keyDecoder: KeyDecoder[Type] = KeyDecoder[String].map(apply(_))
  }
  type Name = Name.Type

  sealed abstract class Element extends Product with Serializable {
    def upcast: Element = this
  }
  object Element {
    final case class Const(value: String) extends Element
    final case class Substitution(name: Name) extends Element

    implicit final val codec: Codec[Element] = deriveCodec[Element]
    implicit final val show: Show[Element] = Show.usingJsonEncoder[Element]
  }

  implicit final val codec: Codec[Template] = deriveCodec[Template]
  implicit final val show: Show[Template] = Show.usingJsonEncoder[Template]
}
