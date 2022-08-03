package peschke.bulk_calls
package config

import config.AppConfig.{ReportingConfig, RetryConfig}
import config.ConfigReader.syntax._
import models.Data.Constants
import models.Template.Name
import services.Reporter.ReportStatus

import cats.effect.IO
import cats.syntax.either._
import com.typesafe.config.{ConfigException, ConfigFactory}
import io.circe.Json

final case class AppConfig(templateConfig: TemplateConfig,
                           responseSpec: ResponseSpec,
                           retryConfig: RetryConfig,
                           requestConfig: RawCallSpec,
                           reportingConfig: ReportingConfig,
                           constants: Constants)
object AppConfig {
  final case class RetryConfig(statuses: Set[ReportStatus]) {
    def shouldRetry(status: ReportStatus): Boolean = statuses.contains(status)
  }
  object RetryConfig {
    implicit val reader: ConfigReader[RetryConfig] = ConfigReader.instance { (config, path) =>
      for {
        subConfig <- config.at(path)
        success <- subConfig.read[Boolean]("success")
        notAttempted <- subConfig.read[Boolean]("not-attempted")
        failedAttempts <- subConfig.read[Boolean]("failed-attempts")
      } yield RetryConfig {
        List(
          Option.when(success)(ReportStatus.Succeeded),
          Option.when(notAttempted)(ReportStatus.FailedWithoutAttempt),
          Option.when(failedAttempts)(ReportStatus.AttemptFailed)
        ).flatten.toSet
      }
    }
  }

  final case class ReportingConfig(cleaned: os.Path, results: os.Path)
  object ReportingConfig {
    implicit val reader: ConfigReader[ReportingConfig] = ConfigReader.instance { (config, path) =>
      for {
        subConfig <- config.at(path)
        cleaned <- subConfig.read[os.Path]("cleaned")
        results <- subConfig.read[os.Path]("results")
      } yield ReportingConfig(cleaned = cleaned, results = results)
    }
  }

  implicit val nameReader: ConfigReader.KeyReader[Name] = ConfigReader.KeyReader.instance(Name(_).asRight)

  implicit val constantsReader: ConfigReader[Constants] = ConfigReader.instance {
    _.read[Map[Name, Json]](_).map(Constants(_))
  }

  def loadIO(file: os.Path): IO[AppConfig] = IO.fromEither(load(file))

  def load(file: os.Path): Either[ConfigException, AppConfig] = {
    for {
      root <- Either.catchOnly[ConfigException](ConfigFactory.parseFile(file.toIO).resolve())
      templateConfig <- root.read[TemplateConfig]("template-config")
      responseSpec <- root.read[ResponseSpec]("success-criteria")
      retryConfig <- root.read[RetryConfig]("retry")
      requestConfig <- root.read[RawCallSpec]("request-config")
      reportingConfig <- root.read[ReportingConfig]("reporting.paths")
      constants <- root.read[Constants]("constants")
    } yield AppConfig(
      templateConfig = templateConfig,
      responseSpec = responseSpec,
      retryConfig = retryConfig,
      requestConfig = requestConfig,
      reportingConfig = reportingConfig,
      constants = constants
    )
  }
}