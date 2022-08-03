package peschke

import cats.Show
import cats.parse.Parser
import cats.parse.Parser.Expectation
import cats.syntax.show._
import cats.syntax.either._
import cats.syntax.foldable._
import io.circe.{Codec, Decoder, Encoder, Json, Printer}
import io.circe.syntax._
import org.http4s.Status

package object bulk_calls {
  implicit final val showHttp4sParseFailure: Show[org.http4s.ParseFailure] = Show.show(_.sanitized)
  implicit final val showCirceParsingFailure: Show[io.circe.ParsingFailure] = Show.show(_.message)

  implicit final val showParseExpectation: Show[Parser.Expectation] = {
    def quote(s: String): String = s"\"$s\""
    def quoteAll(l: List[String]): List[String] = l.map(quote)
    def render(expectation: Expectation): String = expectation match {
      case Expectation.OneOfStr(offset, strs) => s"$offset: expected one of ${quoteAll(strs).mkString(", ")}"
      case Expectation.InRange(offset, lower, upper) => s"$offset: expected in range $lower..$upper"
      case Expectation.StartOfString(offset) => s"$offset: expected start of string"
      case Expectation.EndOfString(offset, length) => s"$offset: expected end of string ($length)"
      case Expectation.Length(offset, expected, actual) => s"$offset: expected string of length $expected, was $actual"
      case Expectation.ExpectedFailureAt(offset, matched) => s"$offset: expected a failure, instead of ${quote(matched)}"
      case Expectation.Fail(offset) => s"$offset: failed"
      case Expectation.FailWith(offset, message) => s"$offset: failed ($message)"
      case Expectation.WithContext(contextStr, expect) => s"[$contextStr] ${render(expect)}"
    }
    Show.show(render)
  }

  implicit final val statusCodec: Codec[Status] = Codec.from(
    Decoder.instance(_.downField("code").as[Int]).emap {
      Status.fromInt(_).leftMap(_.message)
    },
    Encoder.instance { status =>
      Json.obj(
        "code" -> status.code.asJson,
        "reason" -> status.reason.asJson
      )
    }
  )

  implicit final class ShowObjOps(private val show: Show.type) extends AnyVal {
    def usingJsonEncoder[A: Encoder]: Show[A] = show.show { a =>
      a.asJson.printWith(Printer.noSpaces)
    }
  }

  implicit final val showParseError: Show[Parser.Error] = Show.show { e =>
    show"failed at ${e.failedAtOffset}${e.expected.mkString_("\n  ", "\n  ", "\n")}"
  }
}
