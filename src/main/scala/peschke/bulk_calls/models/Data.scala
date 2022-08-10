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

  def empty(id: Identifier): Data = Data(id, Map.empty)

  def ofJson(id: Identifier)(v0: (Name, Json), v1: (Name, Json)*): Data = Data(id, (v0 :: v1.toList).toMap)
  def ofText(id: Identifier)(v0: (Name, String), v1: (Name, String)*): Data =
    Data(
      id,
      (v0 :: v1.toList)
        .map {
          case (name, str) => name -> Json.fromString(str)
        }
        .toMap
    )

  final case class Constants(values: Map[Name, Json])
  object Constants {
    val empty: Constants = Constants(Map.empty)
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