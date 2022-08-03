package peschke.bulk_calls
package config

import config.ConfigReader.syntax._
import config.ResponseSpec.{AcceptableCodes, ResponseCondition}
import models.{JsonPath, When}

import cats.data.NonEmptySet
import cats.parse.{Numbers, Parser}
import cats.syntax.either._
import cats.syntax.show._
import com.typesafe.config.ConfigException
import io.circe.Json
import org.http4s.Status

import scala.util.matching.Regex

final case class ResponseSpec(acceptableCodes: AcceptableCodes,
                              successCondition: ResponseCondition,
                              failureCondition: ResponseCondition)
object ResponseSpec {
  sealed abstract class AcceptableCodes extends Product with Serializable {
    def upcast: AcceptableCodes = this
  }
  object AcceptableCodes {
    case object Anything extends AcceptableCodes
    case object AnySuccess extends AcceptableCodes
    final case class Only(codes: NonEmptySet[Status]) extends AcceptableCodes

    object parsers {
      val comma: Parser[Unit] = Parser.char(',')
      val anything: Parser[AcceptableCodes] = Parser.string("n/a").as(Anything.upcast)
      val anySuccess: Parser[AcceptableCodes] = Parser.string("success").as(AnySuccess.upcast)
      val only: Parser[AcceptableCodes] =
        Parser
          .repUntil(Numbers.digit, comma)
          .string
          .mapFilter(_.toIntOption)
          .mapFilter(Status.fromInt(_).toOption)
          .repSep(comma)
          .map(_.toNes)
          .map(Only(_).upcast)

      val acceptableCodes: Parser[AcceptableCodes] = anything.orElse(anySuccess).orElse(only)
    }

    def from(string: String): Either[Parser.Error, AcceptableCodes] =
      parsers.acceptableCodes.parseAll(string)

    implicit val reader: ConfigReader[AcceptableCodes] = ConfigReader.instance { (config, path) =>
      config.read[String](path).flatMap { raw =>
        AcceptableCodes.from(raw).leftMap { parseError =>
          new ConfigException.BadValue(config.origin(), path, parseError.show)
        }
      }
    }
  }

  final case class ResponseCondition(extractor: Either[Regex, JsonPath], condition: When)
  object ResponseCondition {
    implicit val jsonPathReader: ConfigReader[JsonPath] = ConfigReader.instance { (config, path) =>
      config.at(path)
        .flatMap(_.read[String]("path"))
        .flatMap(JsonPath.from(_).leftMap { parseError =>
          new ConfigException.BadValue(config.origin(), path, parseError.show)
        })
    }

    implicit val whenReader: ConfigReader[When] = ConfigReader.instance { (config, path) =>
      config.read[Json](path).flatMap { json =>
        json.as[When].leftMap { decodeFailure =>
          new ConfigException.BadValue(config.origin(), path, "invalid condition syntax", decodeFailure)
        }
      }
    }

    implicit val responseConditionReader: ConfigReader[ResponseCondition] = ConfigReader.instance { (config, path) =>
      for {
        subConfig <- config.at(path)
        extractor <- subConfig.read[Regex]("regex").map(_.asLeft).orElse {
          subConfig.read[JsonPath]("json").map(_.asRight)
        }
        when <- subConfig.read[When]("when")
      } yield ResponseCondition(extractor, when)
    }
  }

  implicit val responseSpecReader: ConfigReader[ResponseSpec] = ConfigReader.instance { (config, path) =>
    for {
      subConfig <- config.at(path)
      acceptableCodes <- subConfig.read[AcceptableCodes]("codes")
      successCriteria <- subConfig.read[ResponseCondition]("body.success")
      failureCriteria <- subConfig.read[ResponseCondition]("body.failure")
    } yield ResponseSpec(acceptableCodes, successCriteria, failureCriteria)
  }
}