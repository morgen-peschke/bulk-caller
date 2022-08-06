package peschke.bulk_calls
package config

import com.typesafe.config.ConfigException
import config.ConfigReader.syntax._
import config.TemplateConfig.{Placeholders, SubstitutionMarkers}
import io.circe.{Json, Printer}
import peschke.bulk_calls.models.Done

final case class TemplateConfig(allowEmpty: Boolean,
                                doNotUseExponents: Boolean,
                                placeholders: Placeholders,
                                substitutionMarkers: SubstitutionMarkers
                               )
object TemplateConfig {
  final case class Placeholders(json: Json, text: String) {
    def jsonText: String = json.printWith(Printer.noSpaces)
  }
  object Placeholders {
    implicit val reader: ConfigReader[Placeholders] = ConfigReader.instance { (config, path) =>
      for {
        subConfig <- config.at(path)
        json <- subConfig.read[Json]("json")
        text <- subConfig.read[String]("text")
      } yield Placeholders(json, text)
    }
  }
  final case class SubstitutionMarkers(open: String, close: String)
  object SubstitutionMarkers {
    implicit val reader: ConfigReader[SubstitutionMarkers] = ConfigReader.instance { (config, path) =>
      for {
        subConfig <- config.at(path)
        open <- subConfig.read[String]("open")
        close <- subConfig.read[String]("close")
        _ <- Either.cond(
          test = open != close,
          right = Done,
          left = new ConfigException.BadValue(
            config.origin,
            subConfig.fullPath("close"),
            s"close cannot be the same as ${subConfig.fullPath("open")}"
          )
        )
      } yield SubstitutionMarkers(open = open, close = close)
    }
  }

  implicit val reader: ConfigReader[TemplateConfig] = ConfigReader.instance[TemplateConfig] { (config, path) =>
    for {
      subConfig <- config.at(path)
      allowEmpty <- subConfig.read[Boolean]("allow-empty")
      doNotUseExponents <- subConfig.read[Boolean]("do-not-use-exponents")
      placeholders <- subConfig.read[Placeholders]("placeholders")
      substitutionMarkers <- subConfig.read[SubstitutionMarkers]("substitution-markers")
    } yield TemplateConfig(allowEmpty, doNotUseExponents, placeholders, substitutionMarkers)
  }
}
