package peschke.bulk_calls
package utils

import cats.data.{NonEmptyList, Validated}
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.validated._
import cats.syntax.either._
import com.monovore.decline.{Argument, Command, Opts}
import peschke.bulk_calls.utils.CliArgs.Source

final case class CliArgs(data: Source, config: os.Path)
object CliArgs {

  case object ShowVersion

  implicit final val pathArgument: Argument[os.Path] = Argument.from("path") { raw =>
    Validated
      .catchNonFatal[os.Path](os.Path.expandUser(raw, os.pwd))
      .leftMap(_.getMessage.pure[NonEmptyList])
  }

  sealed abstract class Source extends Product with Serializable
  object Source {
    case object StdIn extends Source
    final case class DataFile(path: os.Path) extends Source

    implicit final val argument: Argument[Source] = Argument.from("-|path") {
      case "-" => StdIn.valid
      case raw => pathArgument.read(raw).map(DataFile)
    }
  }

  val dataOpts: Opts[Source] = Opts.option[Source](
    long = "data",
    help =
      """|Source of the data for each call.
         |
         |The expected format is JSONL, with an 'id' field containing a scalar, and a 'data' field containing a JSON
         | object with a key for each name in the substitutions.""".stripMargin
  )

  val configOpts: Opts[os.Path] = Opts.option[os.Path](
    long = "config",
    help =
      """|Source of HOCON config file
         |
         |The expected format is documented in src/main/resources/reference.conf""".stripMargin
  )

  val versionOpts: Opts[ShowVersion.type] = Opts.flag(
    long = "version",
    help = "Print the version and exit"
  ).map(_ => ShowVersion)

  val command: Command[Either[ShowVersion.type,CliArgs]] = Command(
    name = peschke.BuildInfo.name,
    header =
      s"""|Make bulk HTTP calls, with tracing
          |
          |Version: ${peschke.BuildInfo.version}""".stripMargin
  ) {
    val run = (dataOpts, configOpts).mapN(CliArgs.apply(_, _).asRight)
    val version = versionOpts.map(_.asLeft)

    run.orElse(version)
  }
}