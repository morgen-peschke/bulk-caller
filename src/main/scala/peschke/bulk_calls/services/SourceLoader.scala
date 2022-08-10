package peschke.bulk_calls
package services

import models.Data
import models.Template.Name
import services.SourceLoader.MalformedSource
import utils.CliArgs.Source

import cats.Show
import cats.effect.kernel.Sync
import cats.syntax.either._
import cats.syntax.functor._
import fs2.io.file.{Files, Path}
import fs2.text
import io.circe.{Decoder, Encoder, Json}

trait SourceLoader[F[_]] {
  def loadData(source: Source): fs2.Stream[F, Either[MalformedSource,Data]]
  def loadConstants(source: Source): F[Either[MalformedSource, Data.Constants]]
}
object SourceLoader {

  final case class MalformedSource(index: Long, rawText: String, message: String)
  object MalformedSource {
    implicit final val show: Show[MalformedSource] = Show.show {
      case MalformedSource(index, rawText, message) =>
        s"""|Malformed input at line $index: $message
            |============== Source Start =================
            |$rawText
            |============== Source End   =================
            |""".stripMargin
    }

    implicit final val encoder: Encoder[MalformedSource] = io.circe.generic.semiauto.deriveEncoder[MalformedSource]
  }

  def default[F[_]: Sync : Files]: SourceLoader[F] = new Default[F]

  final class Default[F[_]: Sync: Files] extends SourceLoader[F] with Serializable {

    def rawStream(source: Source): fs2.Stream[F, String] =
      source match {
        case Source.StdIn => fs2.io.stdinUtf8[F](4096).through(text.lines)
        case Source.DataFile(path) =>
          Files[F]
            .readAll(Path.fromNioPath(path.toNIO))
            .through(text.utf8.decode)
            .through(text.lines)
      }

    override def loadConstants(source: Source): F[Either[MalformedSource, Data.Constants]] =
      rawStream(source).compile.foldSemigroup.map {
        _.fold(Data.Constants.empty.asRight[MalformedSource]) { rawText =>
          io.circe.parser.parse(rawText)
            .leftMap(e => MalformedSource(-1, rawText, e.message))
            .flatMap { json =>
              Decoder[Map[Name, Json]].decodeJson(json)
                .bimap(
                  e => MalformedSource(-1, rawText, e.message),
                  Data.Constants(_)
                )
            }
        }
      }

    override def loadData(source: Source): fs2.Stream[F, Either[MalformedSource,Data]] =
      rawStream(source).zipWithIndex.map {
        case (rawText, index) =>
          io.circe.parser.parse(rawText)
            .leftMap(e => MalformedSource(index, rawText, e.message))
            .flatMap {
              Decoder[Data].decodeJson(_).leftMap(e => MalformedSource(index, rawText, e.message))
            }
      }

  }
}
