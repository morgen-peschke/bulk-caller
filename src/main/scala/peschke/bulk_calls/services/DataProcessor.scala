package peschke.bulk_calls
package services

import config.AppConfig
import config.AppConfig.RetryConfig
import models.{Data, Done}
import services.SourceLoader.MalformedSource

import cats.effect.MonadCancel
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.show._
import com.typesafe.scalalogging.LazyLogging

trait DataProcessor[F[_]] {
  def process(dataStream: fs2.Stream[F, Either[MalformedSource,Data]]): fs2.Stream[F, Done]
}
object DataProcessor {

  def default[F[_]: SuccessIdentifier](implicit A: ApiCaller[F],
                                       R: Reporter[F],
                                       C: AppConfig,
                                       MC: MonadCancel[F, Throwable]): DataProcessor[F] =
    new Default[F](A, R, C.retryConfig, implicitly[SuccessIdentifier[F]])

  final class Default[F[_]](apiCaller: ApiCaller[F],
                            reporter: Reporter[F],
                            retryConfig: RetryConfig,
                            successIdentifier: SuccessIdentifier[F])(implicit MC: MonadCancel[F, Throwable])
    extends DataProcessor[F] with LazyLogging with Serializable {

    def makeCall(data: Data): F[Done] =
      apiCaller.call(data)
        .flatMap(_.fold(
          reporter.reportAttemptFailure(data.identifier, _),
          _.use { response =>
            successIdentifier
              .checkForSuccess(response)
              .flatMap(reporter.reportAttemptResult(data.identifier, _))
          }))

    override def process(dataStream: fs2.Stream[F, Either[MalformedSource, Data]]): fs2.Stream[F, Done] =
      dataStream.evalMap {
        case Left(malformedSource) =>
          logger.warn(s"Unable to attempt line ${malformedSource.index}")
          reporter.reportUnableToAttempt(malformedSource)
        case Right(data) =>
          reporter.checkPreviousReport(data.identifier).flatMap { reportOpt =>
            reportOpt.fold(makeCall(data)) { report =>
              if (retryConfig.shouldRetry(report)) {
                logger.info(show"Retrying ${data.identifier}, despite previous $report")
                makeCall(data)
              } else {
                logger.info(show"Skipping ${data.identifier}, due to previous $report")
                Done.upcast.pure[F]
              }
            }
          }
      }
  }
}