package peschke.bulk_calls
package models

import models.Data.Identifier
import models.Template.Name
import utils.CirceUtils._

import cats.Show
import cats.syntax.either._
import io.circe.{Decoder, Encoder, Json}

final case class Data(identifier: Identifier, values: Map[Name, Json])
object Data {

  final case class Constants(values: Map[Name, Json])
  object Constants {
    implicit final val decoder: Decoder[Constants] = io.circe.generic.semiauto.deriveDecoder[Constants]
  }

  object Identifier extends supertagged.NewType[Either[BigDecimal, String]] {
    def of(str: String): Type = apply(str.asRight[BigDecimal])

    implicit final val show: Show[Type] = Show.show(raw(_).valueOr(_.bigDecimal.toEngineeringString))
    implicit final val decoder: Decoder[Type] = Decoder.either[BigDecimal, String].map(apply(_))
    implicit final val encoder: Encoder[Type] = Encoder.either[BigDecimal, String].contramap(raw)
  }
  type Identifier = Identifier.Type

  implicit final val decoder: Decoder[Data] = io.circe.generic.semiauto.deriveDecoder[Data]
}