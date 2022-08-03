package peschke.bulk_calls
package models

import models.Data.Identifier
import models.ResultReport.ResultType
import services.SuccessIdentifier.ResponseDetails
import utils.CirceUtils._

import cats.syntax.either._
import io.circe.generic.semiauto._
import io.circe.{Codec, Decoder, Encoder, Json}

final case class ResultReport(id: Identifier,
                              result: ResultType,
                              determinant: Either[String, Json],
                              details: Either[Json,ResponseDetails],
                              errors: List[Json])
object ResultReport {
  sealed abstract class ResultType extends enumeratum.EnumEntry
  object ResultType extends enumeratum.Enum[ResultType] {
    case object UnableToAttempt extends ResultType
    case object AttemptFailed extends ResultType
    case object Success extends ResultType
    case object KnownFailure extends ResultType
    case object UnexpectedStructure extends ResultType
    case object UnableToConfirm extends ResultType

    override def values: IndexedSeq[ResultType] = findValues

    implicit final val decoder: Decoder[ResultType] =
      Decoder[String].emap(ResultType.withNameInsensitiveEither(_).leftMap(_.getMessage))

    implicit final val encoder: Encoder[ResultType] =
      Encoder[String].contramap(_.entryName)
  }

  implicit val determinantCodec: Codec[Either[String, Json]] =
    Codec.from(Decoder.either[String, Json], Encoder.either[String, Json])

  implicit val detailsCodec: Codec[Either[Json, ResponseDetails]] =
    Codec.from(Decoder.either[Json, ResponseDetails], Encoder.either[Json, ResponseDetails])

  implicit final val codec: Codec[ResultReport] = deriveCodec[ResultReport]
}