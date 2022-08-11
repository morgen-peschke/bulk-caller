package peschke.bulk_calls
package services

import cats.Show
import cats.data.NonEmptyList
import cats.effect.kernel.Concurrent
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.show._
import com.typesafe.scalalogging.LazyLogging
import fs2.io.file.{Files, Flags, Path}
import fs2.text
import io.circe.Json
import io.circe.syntax._
import peschke.bulk_calls.config.AppConfig
import peschke.bulk_calls.config.AppConfig.ReportingConfig
import peschke.bulk_calls.models.Data.Identifier
import peschke.bulk_calls.models.ResultReport.ResultType
import peschke.bulk_calls.models.{Done, ResultReport}
import peschke.bulk_calls.services.Reporter.ReportStatus
import peschke.bulk_calls.services.SourceLoader.MalformedSource
import peschke.bulk_calls.services.SuccessIdentifier.{ResponseDetails, Result}

trait Reporter[F[_]] {
  def reportUnableToAttempt(malformedSource: MalformedSource): F[Done]
  def reportAttemptFailure(id: Identifier, failures: NonEmptyList[ApiCaller.CallFailure]): F[Done]
  def reportAttemptResult(id: Identifier, result: SuccessIdentifier.Result): F[Done]
  def checkPreviousReport(id: Identifier): F[Option[ReportStatus]]
}
object Reporter {
  sealed abstract class ReportStatus extends Product with Serializable
  object ReportStatus {
    case object Succeeded extends ReportStatus
    case object FailedWithoutAttempt extends ReportStatus
    case object AttemptFailed extends ReportStatus

    implicit final val show: Show[ReportStatus] = Show.show {
      case Succeeded => "success"
      case FailedWithoutAttempt => "failure before attempt"
      case AttemptFailed => "failed attempt"
    }
  }

  def default[F[_]: Files: Concurrent](implicit AC: AppConfig): Reporter[F] =
    new Default[F](AC.reportingConfig)

  final class Default[F[_]: Files: Concurrent](reportingConfig: ReportingConfig)
    extends Reporter[F] with LazyLogging with Serializable {

    private val successPath = Path.fromNioPath(reportingConfig.results.toNIO) / "succeeded"
    private val failurePath = Path.fromNioPath(reportingConfig.results.toNIO) / "failed"

    def locateResult(id: Identifier): F[Option[ResultReport]] =
      Files[F]
        .walk(successPath)
        .append(Files[F].walk(failurePath))
        .filter(_.endsWith(id.show))
        .head
        .flatMap(Files[F].readAll(_).through(text.utf8.decode))
        .map { raw =>
          io.circe.parser.decode[ResultReport](raw).fold(
            error => {
              logger.error(show"Unable to decode existing report for $id", error)
              none
            },
            _.some
          )
        }
        .compile
        .lastOrError

    def writeSuccess(report: ResultReport): F[Done] = writeReport(report, successPath)
    def writeFailure(report: ResultReport): F[Done] = writeReport(report, failurePath)
    def writeReport(report: ResultReport, path: Path): F[Done] =
      fs2.Stream(report.asJson.compact)
        .through(text.utf8.encode)
        .through(Files[F].writeAll(path / report.id.show, Flags.Write))
        .as(Done.upcast)
        .handleErrorWith { error =>
          logger.error(show"Unable to write report for ${report.id}", error)
          fs2.Stream(Done.upcast)
        }
        .compile
        .last
        .map(_ => Done)

    private val noDeterminant: Either[String, Json] = Json.Null.asRight
    private val noDetails: Either[Json, ResponseDetails] = Json.Null.asLeft

    override def reportUnableToAttempt(malformedSource: MalformedSource): F[Done] =
      writeFailure(ResultReport(
        id = Identifier.of(s"source-line-${malformedSource.index}"),
        result = ResultType.UnableToAttempt,
        determinant = noDeterminant,
        details = noDetails,
        errors = malformedSource.asJson :: Nil
      ))

    override def reportAttemptFailure(id: Identifier, failures: NonEmptyList[ApiCaller.CallFailure]): F[Done] =
      writeFailure(ResultReport(
        id = id,
        result = ResultType.UnableToAttempt,
        determinant = noDeterminant,
        details = noDetails,
        errors = failures.map(_.asJson).toList
      ))

    override def reportAttemptResult(id: Identifier, result: SuccessIdentifier.Result): F[Done] =
      result match {
        case Result.GoodResponse(value, response) =>
          writeSuccess(ResultReport(
            id = id,
            result = ResultType.Success,
            details = response.asRight,
            determinant = value,
            errors = Nil
          ))
        case Result.CannotParse(status, error, body) =>
          writeFailure(ResultReport(
            id = id,
            result = ResultType.UnexpectedStructure,
            determinant = noDeterminant,
            details = ResponseDetails.TextResponse(status, body).asRight,
            errors = error.getMessage.asJson :: Nil
          ))
        case Result.BadStatus(response) =>
          writeFailure(ResultReport(
            id = id,
            result = ResultType.KnownFailure,
            determinant = response.status.asJson.asRight,
            details = response.asRight,
            errors = Nil
          ))
        case Result.BadResponse(value, response) =>
          writeFailure(ResultReport(
            id = id,
            result = ResultType.KnownFailure,
            determinant = value,
            details = response.asRight,
            errors = Nil
          ))
        case Result.UnexpectedStructure(decodingFailures, response) =>
          writeFailure(ResultReport(
            id = id,
            result = ResultType.UnableToConfirm,
            determinant = noDeterminant,
            details = response.asRight,
            errors = decodingFailures.getMessage.asJson :: Nil
          ))
        case Result.Unconfirmed(response) =>
          writeFailure(ResultReport(
            id = id,
            result = ResultType.UnableToConfirm,
            details = response.asRight,
            determinant = noDeterminant,
            errors = Nil
          ))
      }

    override def checkPreviousReport(id: Identifier): F[Option[ReportStatus]] =
      locateResult(id).map(_.map { report =>
        report.result match {
          case ResultType.Success => ReportStatus.Succeeded
          case ResultType.UnableToAttempt => ReportStatus.FailedWithoutAttempt
          case ResultType.AttemptFailed |
               ResultType.KnownFailure |
               ResultType.UnexpectedStructure |
               ResultType.UnableToConfirm => ReportStatus.AttemptFailed
        }
      })
  }
}