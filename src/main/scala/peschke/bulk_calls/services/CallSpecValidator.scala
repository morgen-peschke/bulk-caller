package peschke.bulk_calls
package services

import config.RawCallSpec
import config.RawCallSpec._
import models.CallSpec
import models.CallSpec._
import services.CallSpecValidator.InvalidCallSpec
import services.CallSpecValidator.InvalidCallSpec._
import services.TemplateParser.syntax._

import cats.data.ValidatedNel
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.show._
import cats.syntax.traverse._
import cats.syntax.validated._
import cats.{Monad, Show}

trait CallSpecValidator[F[_]] {
  def validate(rawCallSpec: RawCallSpec): F[ValidatedNel[InvalidCallSpec, CallSpec]]
}
object CallSpecValidator {

  sealed abstract class InvalidCallSpec extends Product with Serializable {
    def upcast: InvalidCallSpec = this
  }
  object InvalidCallSpec {
    final case class InvalidUrl(error: TemplateParser.InvalidTemplate) extends InvalidCallSpec
    final case class InvalidParamName(raw: RawQueryParamName, error: TemplateParser.InvalidTemplate)
      extends InvalidCallSpec

    final case class InvalidParamValue(param: RawQueryParamName, error: TemplateParser.InvalidTemplate)
      extends InvalidCallSpec

    final case class InvalidHeaderName(raw: RawHeaderName, error: TemplateParser.InvalidTemplate)
      extends InvalidCallSpec

    final case class InvalidHeaderValue(header: RawHeaderName, error: TemplateParser.InvalidTemplate)
      extends InvalidCallSpec

    final case class InvalidBody(error: TemplateParser.InvalidTemplate) extends InvalidCallSpec

    implicit final val show: Show[InvalidCallSpec] = Show.show {
      case InvalidUrl(error) => show"URL is invalid: $error"
      case InvalidParamName(raw, error) => show"QueryParam name ($raw) is invalid: $error"
      case InvalidParamValue(param, error) =>  show"QueryParam value for $param is invalid: $error"
      case InvalidHeaderName(raw, error) => show"Header name ($raw) is invalid: $error"
      case InvalidHeaderValue(header, error) => show"Header value for $header is invalid: $error"
      case InvalidBody(error) => show"Body is invalid: $error"
    }
  }

  def default[F[_]: TemplateParser : Monad]: CallSpecValidator[F] = new Default[F]

  final class Default[F[_]: TemplateParser : Monad] extends CallSpecValidator[F] with Serializable {

    def parseUrl(url: RawUrl): F[ValidatedNel[InvalidCallSpec, Url]] =
      url.parse[F].map(_.bimap(_.map(InvalidUrl(_).upcast), Url(_)).toValidated)

    def parseQueryParam(queryParam: (RawQueryParamName, RawQueryParamValue))
    : F[ValidatedNel[InvalidCallSpec, (QueryParamName, QueryParamValue)]] = queryParam match {
      case (name, value) =>
        for {
          validatedName <- name.parse[F].map {
            _.bimap(_.map(InvalidParamName(name, _).upcast), QueryParamName(_)).toValidated
          }
          validatedValue <- value.parse[F].map {
            _.bimap(_.map(InvalidParamValue(name, _).upcast), QueryParamValue(_)).toValidated
          }
        } yield validatedName.product(validatedValue)
    }

    def parseHeader(header: (RawHeaderName, RawHeaderValue))
    : F[ValidatedNel[InvalidCallSpec, (HeaderName, HeaderValue)]] = header match {
      case (name, value) =>
        for {
          validatedName <- name.parse[F].map {
            _.bimap(_.map(InvalidHeaderName(name, _).upcast), HeaderName(_)).toValidated
          }
          validatedValue <- value.parse[F].map {
            _.bimap(_.map(InvalidHeaderValue(name, _).upcast), HeaderValue(_)).toValidated
          }
        } yield validatedName.product(validatedValue)
    }

    def parseBody(raw: RawBody): F[ValidatedNel[InvalidCallSpec, Body]] =
      raw match {
        case RawBody.JsonBody(template) =>
          template.parse[F].map(_.bimap(_.map(InvalidBody(_).upcast), Body.JsonBody(_).upcast).toValidated)
        case RawBody.TextBody(template) =>
          template.parse[F].map(_.bimap(_.map(InvalidBody(_).upcast), Body.TextBody(_).upcast).toValidated)
        case RawBody.EmptyBody => Body.EmptyBody.upcast.validNel[InvalidCallSpec].pure[F]
      }

    override def validate(rawCallSpec: RawCallSpec): F[ValidatedNel[InvalidCallSpec, CallSpec]] =
      rawCallSpec match {
        case RawCallSpec.Get(url, queryParams, headers) =>
          for {
            validatedUrl <- parseUrl(url)
            validatedQueryParams <- queryParams.traverse(parseQueryParam).map(_.sequence)
            validatedHeaders <- headers.traverse(parseHeader).map(_.sequence)
          } yield (validatedUrl, validatedQueryParams, validatedHeaders).mapN(CallSpec.Get)

        case RawCallSpec.Post(url, queryParams, headers, body) =>
          for {
            validatedUrl <- parseUrl(url)
            validatedQueryParams <- queryParams.traverse(parseQueryParam).map(_.sequence)
            validatedHeaders <- headers.traverse(parseHeader).map(_.sequence)
            validatedBody <- parseBody(body)
          } yield (validatedUrl, validatedQueryParams, validatedHeaders, validatedBody).mapN(CallSpec.Post)
      }
  }
}
