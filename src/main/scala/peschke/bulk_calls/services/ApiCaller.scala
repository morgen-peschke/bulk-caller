package peschke.bulk_calls
package services

import models.CallSpec._
import models.{CallSpec, Data, Template}
import services.ApiCaller.CallFailure
import services.ApiCaller.CallFailure._
import services.TemplateExpander.syntax._

import cats.Show
import cats.data.{NonEmptyList, ValidatedNel}
import cats.effect.{Concurrent, Resource}
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.show._
import cats.syntax.traverse._
import cats.syntax.validated._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Codec, Decoder, Encoder, Json}
import org.http4s.client._
import org.http4s.client.dsl.MethodOps
import org.http4s.headers.`Content-Length`
import org.http4s.{EmptyBody, Entity, EntityEncoder, Headers, Method, QueryParamEncoder, QueryParamKeyLike, QueryParameterKey, Request, Response, Uri}

trait ApiCaller[F[_]] {
  def call(data: Data): F[ValidatedNel[CallFailure, Resource[F, Response[F]]]]
}
object ApiCaller {
  sealed abstract class CallFailure extends Product with Serializable {
    def upcast: CallFailure = this
  }
  object CallFailure {
    final case class UnableToExpandUrl(raw: CallSpec.Url,
                                       errors: NonEmptyList[TemplateExpander.ExpansionError])
      extends CallFailure
    object UnableToExpandUrl {
      implicit final val codec: Codec[UnableToExpandUrl] = deriveCodec[UnableToExpandUrl].withIdentifier
    }

    final case class InvalidExpandedUrl(raw: CallSpec.Url, expanded: String, error: String) extends CallFailure
    object InvalidExpandedUrl {
      implicit final val codec: Codec[InvalidExpandedUrl] = deriveCodec[InvalidExpandedUrl].withIdentifier
    }

    final case class UnableToExpandQueryParamName(raw: QueryParamName,
                                                  errors: NonEmptyList[TemplateExpander.ExpansionError])
      extends CallFailure
    object UnableToExpandQueryParamName {
      implicit final val codec: Codec[UnableToExpandQueryParamName] =
        deriveCodec[UnableToExpandQueryParamName].withIdentifier
    }

    final case class UnableToExpandQueryParamValue(raw: QueryParamValue,
                                                   errors: NonEmptyList[TemplateExpander.ExpansionError])
      extends CallFailure
    object UnableToExpandQueryParamValue {
      implicit final val codec: Codec[UnableToExpandQueryParamValue] =
        deriveCodec[UnableToExpandQueryParamValue].withIdentifier
    }

    final case class UnableToExpandHeaderName(raw: HeaderName,
                                              errors: NonEmptyList[TemplateExpander.ExpansionError])
      extends CallFailure
    object UnableToExpandHeaderName {
      implicit final val codec: Codec[UnableToExpandHeaderName] =
        deriveCodec[UnableToExpandHeaderName].withIdentifier
    }

    final case class UnableToExpandHeaderValue(raw: HeaderValue,
                                               errors: NonEmptyList[TemplateExpander.ExpansionError])
      extends CallFailure
    object UnableToExpandHeaderValue {
      implicit final val codec: Codec[UnableToExpandHeaderValue] =
        deriveCodec[UnableToExpandHeaderValue].withIdentifier
    }

    final case class UnableToExpandBody(raw: Template,
                                        errors: NonEmptyList[TemplateExpander.ExpansionError])
      extends CallFailure
    object UnableToExpandBody {
      implicit final val codec: Codec[UnableToExpandBody] = deriveCodec[UnableToExpandBody].withIdentifier
    }

    implicit final val codec: Codec[CallFailure] = Codec.from(
      List[Decoder[CallFailure]](
        Decoder[UnableToExpandUrl].widen,
        Decoder[InvalidExpandedUrl].widen,
        Decoder[UnableToExpandQueryParamName].widen,
        Decoder[UnableToExpandQueryParamValue].widen,
        Decoder[UnableToExpandHeaderName].widen,
        Decoder[UnableToExpandHeaderValue].widen,
        Decoder[UnableToExpandBody].widen
      ).reduceLeft(_ or _),
      Encoder.instance {
        case cf @ UnableToExpandUrl(_, _) => cf.asJson
        case cf @ InvalidExpandedUrl(_, _, _) => cf.asJson
        case cf @ UnableToExpandQueryParamName(_, _) => cf.asJson
        case cf @ UnableToExpandQueryParamValue(_, _) => cf.asJson
        case cf @ UnableToExpandHeaderName(_, _) => cf.asJson
        case cf @ UnableToExpandHeaderValue(_, _) => cf.asJson
        case cf @ UnableToExpandBody(_, _) => cf.asJson
      }
    )

    implicit final val show: Show[CallFailure] = Show.usingJsonEncoder[CallFailure]
  }

  def default[F[_]: Concurrent: TemplateExpander](client: Client[F], callSpec: CallSpec): ApiCaller[F] =
    new Default[F](client, callSpec)

  object ExpandedQueryParamName extends supertagged.NewType[String] {
    implicit final val encoder: QueryParamKeyLike[Type] = t => QueryParameterKey(ExpandedQueryParamName.raw(t))
  }
  type ExpandedQueryParamName = ExpandedQueryParamName.Type

  object ExpandedQueryParamValue extends supertagged.NewType[String] {
    implicit final val encoder: QueryParamEncoder[Type] = QueryParamEncoder[String].contramap(raw)
  }
  type ExpandedQueryParamValue = ExpandedQueryParamValue.Type

  final class Default[F[_]: Concurrent: TemplateExpander](client: Client[F], callSpec: CallSpec)
    extends ApiCaller[F] with Serializable {

    implicit def enableMethodOps(m: Method): MethodOps[F] = new MethodOps[F](m)

    private val textEncoder: EntityEncoder[F, String] = EntityEncoder[F, String]
    private val jsonEncoder: EntityEncoder[F, Json] = org.http4s.circe.jsonEncoder[F]
    private val emptyBody: F[ValidatedNel[CallFailure, (Headers, Entity[F])]] =
      (Headers.empty -> Entity[F](EmptyBody, Some(0L))).validNel[CallFailure].pure[F]

    def generateUrl(data: Data): F[ValidatedNel[CallFailure, Uri]] =
      CallSpec.Url
        .raw(callSpec.url)
        .expandText(data)
        .map(_.leftMap(UnableToExpandUrl(callSpec.url, _).upcast.pure[NonEmptyList]))
        .map(_.andThen { expandedUrl =>
          Uri.fromString(expandedUrl)
            .leftMap(pf => InvalidExpandedUrl(callSpec.url, expandedUrl, pf.show).upcast)
            .toValidatedNel
        })

    def generateQueryParams(data: Data)
    : F[ValidatedNel[CallFailure, List[(ExpandedQueryParamName, List[ExpandedQueryParamValue])]]] = {
      def expandName(name: QueryParamName): F[ValidatedNel[CallFailure, ExpandedQueryParamName]] =
        QueryParamName.raw(name)
          .expandText(data)
          .map(_.bimap(
            UnableToExpandQueryParamName(name, _).upcast.pure[NonEmptyList],
            ExpandedQueryParamName(_)
          ))

      def expandValue(value: QueryParamValue): F[ValidatedNel[CallFailure, List[ExpandedQueryParamValue]]] =
        QueryParamValue.raw(value)
          .expandTextWithRepetition(data)
          .map(_.bimap(
            UnableToExpandQueryParamValue(value, _).upcast.pure[NonEmptyList],
            ExpandedQueryParamValue.lift(_)
          ))

      callSpec.queryParams
        .traverse {
          case (nameTemplate, valueTemplate) =>
            for {
              name <- expandName(nameTemplate)
              value <- expandValue(valueTemplate)
            } yield name.product(value)
        }
        .map(_.sequence)
    }

    def generateCustomHeaders(data: Data): F[ValidatedNel[CallFailure, Headers]] = {
      def expandName(name: HeaderName): F[ValidatedNel[CallFailure, String]] =
        HeaderName.raw(name)
          .expandText(data)
          .map(_.leftMap(UnableToExpandHeaderName(name, _).upcast.pure[NonEmptyList]))

      def expandValue(value: HeaderValue): F[ValidatedNel[CallFailure, String]] =
        HeaderValue.raw(value)
          .expandText(data)
          .map(_.leftMap(UnableToExpandHeaderValue(value, _).upcast.pure[NonEmptyList]))

      callSpec.extraHeaders
        .traverse {
          case (nameTemplate, valueTemplate) =>
            for {
              name <- expandName(nameTemplate)
              value <- expandValue(valueTemplate)
            } yield name.product(value)
        }
        .map(_.sequence.map(Headers(_)))
    }

    def generateBody(data: Data): F[ValidatedNel[CallFailure, (Headers, Entity[F])]] =
      callSpec.body match {
        case Body.EmptyBody => emptyBody
        case Body.JsonBody(template) =>
          template
            .expandJson[F](data)
            .map {
              _.leftMap(UnableToExpandBody(template, _).upcast.pure[NonEmptyList])
                .map(jsonEncoder.headers -> jsonEncoder.toEntity(_))
            }
        case Body.TextBody(template) =>
          template
            .expandText[F](data)
            .map {
              _.leftMap(UnableToExpandBody(template, _).upcast.pure[NonEmptyList])
                .map(textEncoder.headers -> textEncoder.toEntity(_))
            }
      }

    def buildRequest(data: Data): F[ValidatedNel[CallFailure, Request[F]]] =
      for {
        validatedUri <- generateUrl(data)
        validatedQueryParams <- generateQueryParams(data)
        validatedHeaders <- generateCustomHeaders(data)
        validatedBody <- generateBody(data)
      } yield (validatedUri, validatedQueryParams, validatedHeaders, validatedBody).mapN {
        case (uri, queryParams, extraHeaders, (bodyHeaders, bodyEntity)) =>
          val defaultHeaders =
            bodyEntity.length
              .flatMap(`Content-Length`.fromLong(_).toOption)
              .fold(bodyHeaders)(bodyHeaders.put(_))

          Request[F](
            method = callSpec.method,
            uri = queryParams.foldLeft(uri) {
              case (uri, (name, values)) => values.foldLeft(uri)(_.withQueryParam(name, _))
            },
            headers = defaultHeaders ++ extraHeaders,
            body = bodyEntity.body
          )
      }

    override def call(data: Data): F[ValidatedNel[CallFailure, Resource[F, Response[F]]]] = {
      buildRequest(data).map(_.map { request =>
        client.run(request)
      })
    }
  }
}