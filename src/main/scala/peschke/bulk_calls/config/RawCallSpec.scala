package peschke.bulk_calls
package config

import config.ConfigReader.syntax._
import services.TemplateParser.IsRawTemplate

import cats.Show
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.show._
import com.monovore.decline.Argument
import com.typesafe.config.ConfigException
import org.http4s.Method

sealed abstract class RawCallSpec extends Product with Serializable
object RawCallSpec {
  abstract class StringArg extends supertagged.NewType[String] {
    implicit final val argument: Argument[Type] = Argument[String].map(apply(_))
    implicit final val isRawTemplate: IsRawTemplate[Type] = IsRawTemplate.instance(raw)
    implicit final val show: Show[Type] = Show.show(t => s"\"${raw(t)}\"")
    implicit final val reader: ConfigReader[Type] = ConfigReader.instance { (config, path) =>
      config.read[String](path).map(apply(_))
    }
    implicit final val keyReader: ConfigReader.KeyReader[Type] =
      ConfigReader.KeyReader.instance(apply(_).asRight)
  }

  object RawUrl extends StringArg
  type RawUrl = RawUrl.Type

  object RawQueryParamName extends StringArg
  type RawQueryParamName = RawQueryParamName.Type

  object RawQueryParamValue extends StringArg
  type RawQueryParamValue = RawQueryParamValue.Type

  object RawHeaderName extends StringArg
  type RawHeaderName = RawHeaderName.Type

  object RawHeaderValue extends StringArg
  type RawHeaderValue = RawHeaderValue.Type

  object RawTemplate extends StringArg
  type RawTemplate = RawTemplate.Type

  sealed abstract class RawBody extends Product with Serializable {
    def upcast: RawBody = this
  }
  object RawBody {
    final case class JsonBody(template: RawTemplate) extends RawBody
    final case class TextBody(template: RawTemplate) extends RawBody
    final case object EmptyBody extends RawBody

    implicit val reader: ConfigReader[Option[RawBody]] = ConfigReader.instance { (config, path) =>
      for {
        subConfig <- config.at(path)
        jsonOpt <- subConfig.readOpt[RawTemplate]("json").map(_.map(JsonBody(_).upcast))
        textOpt <- subConfig.readOpt[RawTemplate]("text").map(_.map(TextBody(_).upcast))
        omitted <- subConfig.readOpt[Boolean]("omitted").flatMap(_.fold(none[RawBody].asRight[ConfigException]){
          case true => EmptyBody.upcast.some.asRight
          case false => new ConfigException.BadValue(config.origin, path, "must be 'true'").asLeft
        })
        rawBodyOpt <- (jsonOpt, textOpt, omitted) match {
          case (Some(body), None, None) => body.some.asRight
          case (None, Some(body), None) => body.some.asRight
          case (None, None, Some(body)) => body.some.asRight
          case (None, None, None) => none[RawBody].asRight
          case _ => new ConfigException.BadValue(
            config.origin,
            path,
            "Only one key may be set below this path"
          ).asLeft
        }
      } yield rawBodyOpt
    }
  }

  final case class Get(url: RawUrl,
                       queryParams: List[(RawQueryParamName, RawQueryParamValue)],
                       extraHeaders: List[(RawHeaderName, RawHeaderValue)]) extends RawCallSpec

  final case class Post(url: RawUrl,
                        queryParams: List[(RawQueryParamName, RawQueryParamValue)],
                        extraHeaders: List[(RawHeaderName, RawHeaderValue)],
                        body: RawBody) extends RawCallSpec

  implicit val methodReader: ConfigReader[Method] = ConfigReader.instance { (config, path) =>
    config.read[String](path).flatMap {
      Method.fromString(_).leftMap { error =>
        new ConfigException.BadValue(config.origin, path, error.show, error)
      }
    }
  }

  implicit val queryParamsReader: ConfigReader[List[(RawQueryParamName, RawQueryParamValue)]] =
    ConfigReader.instance(_.read[Map[RawQueryParamName, RawQueryParamValue]](_).map(_.toList))

  implicit val headerReader: ConfigReader[List[(RawHeaderName, RawHeaderValue)]] =
    ConfigReader.instance(_.read[Map[RawHeaderName, RawHeaderValue]](_).map(_.toList))

  implicit val reader: ConfigReader[RawCallSpec] = ConfigReader.instance { (config, path) =>
    for {
      subConfig <- config.at(path)
      url <- subConfig.read[RawUrl]("url-template")
      queryParams <- subConfig.read[List[(RawQueryParamName, RawQueryParamValue)]]("query-params")
      headers <- subConfig.read[List[(RawHeaderName, RawHeaderValue)]]("headers")
      rawBodyOpt <- subConfig.read[Option[RawBody]]("body")
      callSpec <- subConfig.read[Method]("method").flatMap {
        case Method.GET =>
          rawBodyOpt
            .toLeft(Get(url, queryParams, headers))
            .leftMap { _ =>
              new ConfigException.BadValue(
                config.origin(),
                subConfig.fullPath("body"),
                "When method is GET, 'body' must be an empty config object"
              )
            }
        case Method.POST =>
          rawBodyOpt
            .map(Post(url, queryParams, headers, _))
            .toRight {
              new ConfigException.BadValue(
                config.origin(),
                subConfig.fullPath("body"),
                "When method is POST, 'body' must be specified"
              )
            }
        case method => new ConfigException.BadValue(
          config.origin(),
          subConfig.fullPath("method"),
          show"Unsupported HTTP method: $method"
        ).asLeft
      }
    } yield callSpec
  }
}
