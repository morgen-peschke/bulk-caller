package peschke.bulk_calls

import config.AppConfig
import models.CallSpec
import services._
import utils.CliArgs
import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.applicative._
import cats.syntax.foldable._
import org.http4s.circe._
import org.http4s.ember.client.EmberClientBuilder
import peschke.BuildInfo
import peschke.bulk_calls.utils.CliArgs.ShowVersion

object Main extends IOApp {

  def callSpecIO(implicit appConfig: AppConfig, TP: TemplateParser[IO]): IO[CallSpec] =
    CallSpecValidator.default[IO].validate(appConfig.requestConfig)
      .flatMap {
        _.map(_.pure[IO]).valueOr { errors =>
          IO.raiseError(new IllegalArgumentException(
            s"Illegal call specification: ${errors.mkString_("\n  ", "\n  ", "")}"
          ))
        }
      }

  def run(args: List[String]): IO[ExitCode] =
    CliArgs.command.parse(args, sys.env) match {
      case Left(help) => Console[IO].errorln(help).as(ExitCode.Error)
      case Right(Left(ShowVersion)) => Console[IO].println(BuildInfo.version).as(ExitCode.Success)
      case Right(Right(cliArgs)) =>
        implicit val sourceLoader: SourceLoader[IO] = SourceLoader.default[IO]

        AppConfig.loadIO(cliArgs.config).flatMap { implicit appConfig =>
          implicit val whenChecker: WhenChecker[IO] = WhenChecker.default[IO]
          implicit val templateParserValidatedNel: TemplateParser[IO] = TemplateParser.default[IO]
          implicit val successIdentifier: SuccessIdentifier[IO] = SuccessIdentifier.default[IO]

          callSpecIO.flatMap { callSpec =>
            EmberClientBuilder.default[IO].build.use { client =>
                implicit val templateExpander: TemplateExpander[IO] = TemplateExpander.default[IO]
                implicit val apiCaller: ApiCaller[IO] = ApiCaller.default[IO](client, callSpec)
                implicit val reporter: Reporter[IO] = Reporter.default[IO]

                DataProcessor.default[IO]
                  .process(sourceLoader.loadData(cliArgs.data))
                  .compile
                  .foldMonoid
                  .as(ExitCode.Success)
              }
            }
        }
    }
}
