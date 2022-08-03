package peschke.bulk_calls
package services

import config.ResponseSpec.{AcceptableCodes, ResponseCondition}
import config.{AppConfig, ResponseSpec}
import models.{JsonPath, When}
import services.SuccessIdentifier.Result
import services.SuccessIdentifier.Result._
import services.WhenChecker.syntax._
import utils.CirceUtils._

import cats.effect.Concurrent
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monadError._
import cats.syntax.option._
import cats.syntax.traverse._
import fs2.text
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.http4s.{EntityDecoder, Response, Status}

import scala.util.matching.Regex

trait SuccessIdentifier[F[_]] {
  def checkForSuccess(response: Response[F]): F[Result]
}
object SuccessIdentifier {
  sealed abstract class Result extends Product with Serializable {
    def upcast: Result = this
  }
  object Result {
    final case class CannotParse(status: Status, error: Throwable, response: String) extends Result
    final case class BadStatus(response: ResponseDetails) extends Result
    final case class BadResponse(value: Either[String, Json], response: ResponseDetails) extends Result
    final case class GoodResponse(value: Either[String, Json], response: ResponseDetails) extends Result
    final case class UnexpectedStructure(decodingFailures: DecodingFailure, response: ResponseDetails) extends Result
    final case class Unconfirmed(response: ResponseDetails) extends Result
  }

  sealed abstract class ResponseDetails extends Product with Serializable {
    def status: Status
    def simple: ResponseDetails
  }
  object ResponseDetails {
    sealed trait Complex extends Product with Serializable {
      def status: Status
      def complex: Complex = this
      def simple: ResponseDetails
    }

    final case class JsonResponse(status: Status, body: Json) extends ResponseDetails with Complex {
      override def simple: ResponseDetails = this
    }
    object JsonResponse {
      implicit final val codec: Codec[JsonResponse] = deriveCodec[JsonResponse].withIdentifier
    }

    final case class ComplexTextResponse(status: Status, body: String, jsonParseError: Throwable)
      extends Complex {
      override def simple: ResponseDetails = TextResponse(status, body)
    }

    final case class NoBody(status: Status) extends ResponseDetails with Complex {
      override def simple: ResponseDetails = this
    }
    object NoBody {
      implicit final val codec: Codec[NoBody] = deriveCodec[NoBody].withIdentifier
    }

    final case class TextResponse(status: Status, body: String)
      extends ResponseDetails {
      override def simple: ResponseDetails = this
    }
    object TextResponse {
      implicit final val codec: Codec[TextResponse] = deriveCodec[TextResponse].withIdentifier
    }

    implicit final val codec: Codec[ResponseDetails] = Codec.from(
      List[Decoder[ResponseDetails]](
        Decoder[JsonResponse].widen,
        Decoder[NoBody].widen,
        Decoder[TextResponse].widen
      ).reduceLeft(_ or _),
      Encoder.instance {
        case rd @ JsonResponse(_, _) => rd.asJson
        case rd @ NoBody(_) => rd.asJson
        case rd @ TextResponse(_, _) => rd.asJson
      }
    )
  }

  def default[F[_]](implicit C: Concurrent[F],
                    ED: EntityDecoder[F, Json],
                    appConfig: AppConfig,
                    WC: WhenChecker[F])
  : SuccessIdentifier[F] =
    new Default[F](appConfig.responseSpec)

  final class Default[F[_]](spec: ResponseSpec)(implicit C: Concurrent[F],
                                                ED: EntityDecoder[F, Json],
                                                WC: WhenChecker[F])
    extends SuccessIdentifier[F] with Serializable {

    private def noErrorYet: F[Option[Result]] = none[Result].pure[F]

    def extractDetails(response: Response[F]): F[Either[Result, ResponseDetails.Complex]] = {
      val status = response.status

      def hasBodyCheck = response.body.compile.last.map(_.isEmpty)

      def fallBackToHexEncodedBody = response.body.through(text.hex.encode).compile.foldMonoid

      def makeCannotParseResult(error: Throwable): F[Either[Result, ResponseDetails.Complex]] =
        fallBackToHexEncodedBody.map(CannotParse(status, error, _).upcast.asLeft[ResponseDetails.Complex])

      def makeTextResponse(text: String, error: Throwable): F[Either[Result, ResponseDetails.Complex]] =
        ResponseDetails.ComplexTextResponse(status, text, error).complex.asRight[Result].pure[F]

      hasBodyCheck.flatMap { doesNotHaveBody =>
        if (doesNotHaveBody) ResponseDetails.NoBody(status).complex.asRight[Result].pure[F]
        else response.as[Json].redeemWith(
          jsonError => response.as[String].redeemWith(makeCannotParseResult, makeTextResponse(_, jsonError)),
          ResponseDetails.JsonResponse(status, _).complex.asRight[Result].pure[F]
        )
      }
    }

    def checkStatus(details: ResponseDetails.Complex): F[Option[Result]] =
        spec.acceptableCodes match {
          case AcceptableCodes.Anything => noErrorYet
          case AcceptableCodes.AnySuccess =>
            if (details.status.isSuccess) noErrorYet
            else BadStatus(details.simple).upcast.some.pure[F]
          case AcceptableCodes.Only(codes) =>
            if (codes.contains(details.status)) noErrorYet
            else BadStatus(details.simple).upcast.some.pure[F]
        }

    def extractValue(details: ResponseDetails.Complex, extractor: Either[Regex, JsonPath]): Option[ExtractionResult] = {
      def checkRegex(regex: Regex, body: String): Option[ExtractionResult] =
        regex.findFirstMatchIn(body)
          .map { regexMatch =>
            FoundValue.from(regexMatch.subgroups.headOption.getOrElse(""))
          }
          .map(ExtractionResult.from)

      extractor match {
        case Left(regex) => details match {
          case ResponseDetails.ComplexTextResponse(_, body, _) => checkRegex(regex, body)
          case ResponseDetails.JsonResponse(_, body) => checkRegex(regex, body.printWith(Printer.noSpaces))
          case ResponseDetails.NoBody(_) => none[ExtractionResult]
        }
        case Right(jsonPath) => details match {
          case ResponseDetails.JsonResponse(_, body) =>
            jsonPath.extract(body)
              .fold(
                decodeFailure => ExtractionResult.from(UnexpectedStructure(decodeFailure, details.simple)).some,
                _.map(value => ExtractionResult.from(FoundValue.from(value)))
              )
          case ResponseDetails.ComplexTextResponse(status, body, jsonParseError) =>
            ExtractionResult.from(CannotParse(status, jsonParseError, body)).some
          case ResponseDetails.NoBody(_) => none[ExtractionResult]
        }
      }
    }

    def checkCondition(details: ResponseDetails.Complex,
                       condition: ResponseCondition,
                       makeResult: (Either[String, Json], ResponseDetails) => Result
                      ): F[Option[Result]] =
      extractValue(details, condition.extractor)
        .traverse { result =>
          ExtractionResult
            .raw(result)
            .traverse { foundValue =>
              foundValue
                .check(condition.condition)
                .map { conditionMatches =>
                  Option.when[Result](conditionMatches) {
                    makeResult(FoundValue.raw(foundValue), details.simple)
                  }
                }
            }
        }
        .map(_.flatMap(_.leftMap(_.some).merge))

    def checkResponse(details: ResponseDetails.Complex): F[Option[Result]] =
       checkCondition(details, spec.successCondition, GoodResponse)
        .flatMap { maybeSuccess =>
          maybeSuccess.map(_.some.pure[F]).getOrElse {
            checkCondition(details, spec.failureCondition, BadResponse)
          }
        }

    override def checkForSuccess(response: Response[F]): F[Result] =
      extractDetails(response).flatMap {
        case Left(unableToParse) => unableToParse.pure[F]
        case Right(details) =>
          for {
            statusError <- checkStatus(details)
            resultError <- checkResponse(details)
          } yield statusError.orElse(resultError).getOrElse(Unconfirmed(details.simple).upcast)
      }
  }

  object FoundValue extends supertagged.NewType[Either[String, Json]] {
    def from(string: String): Type = apply(string.asLeft[Json])
    def from(json: Json): Type = apply(json.asRight[String])

    implicit final class Ops(private val foundValue: Type) extends AnyVal {
      def check[F[_]: WhenChecker](when: When): F[Boolean] = raw(foundValue).fold(when.check(_), when.check(_))
    }
  }
  type FoundValue = FoundValue.Type

  object ExtractionResult extends supertagged.NewType[Either[Result, FoundValue]] {
    def from(result: Result): Type = apply(result.asLeft[FoundValue])
    def from(foundValue: FoundValue): Type = apply(foundValue.asRight[Result])
  }
  type ExtractionResult = ExtractionResult.Type
}