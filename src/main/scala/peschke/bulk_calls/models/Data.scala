package peschke.bulk_calls
package models

import models.Data.Identifier
import models.Template.Name

import cats.Show
import cats.syntax.either._
import io.circe.{Decoder, Encoder, Json}

/**
  * The data used to fill in/expand a [[CallSpec]] into what's needed for an actual call.
  *
  * @param identifier A name or number used to identify the call in logs
  * @param values Name & values pairs used for substitutions. Extra values are ok, missing value handling will depend
  *               on the constants and configs.
  */
final case class Data(identifier: Identifier, values: Map[Name, Json])
object Data {

  def empty(id: Identifier): Data = Data(id, Map.empty)

  def of(id: Identifier)(v0: (Name, Json), v1: (Name, Json)*): Data = Data(id, (v0 :: v1.toList).toMap)

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