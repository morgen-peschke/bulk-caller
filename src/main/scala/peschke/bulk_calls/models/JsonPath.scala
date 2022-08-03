package peschke.bulk_calls
package models

import cats.Show
import cats.parse.{Numbers, Parser}
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.show._
import io.circe.Decoder.Result
import io.circe.{CursorOp, Decoder, Encoder, Json}

final case class JsonPath(segments: List[CursorOp], raw: String) {
  def extract(json: Json): Result[Option[Json]] =
    json.hcursor.replay(segments).as[Option[Json]]
}
object JsonPath {
  object parsers {
    val openBrace: Parser[Unit] = Parser.char('[')
    val closeBrace: Parser[Unit] = Parser.char(']')
    val quote: Parser[Unit] = Parser.char('"')
    val identifier: Parser[String] = {
      val initial = Parser.charWhere(_.isLetter).orElse(Parser.char('_'))
      val rest = initial.orElse(Numbers.digit)
      Parser.char('.') *> (initial *> rest.rep0).string
    }

    val explicitField: Parser[String] =
      Parser.until(quote).surroundedBy(quote)

    val arrayIndex: Parser[String] = Numbers.nonNegativeIntString

    val cursorOp: Parser[CursorOp] = {
      val downFieldSimple: Parser[CursorOp] = identifier.map(CursorOp.DownField)
      val downFieldOrArrayComplex: Parser[CursorOp] =
        explicitField.map(CursorOp.DownField)
          .orElse(arrayIndex.mapFilter(_.toIntOption).map(CursorOp.DownN))
          .between(openBrace, closeBrace)

      downFieldSimple.orElse(downFieldOrArrayComplex)
    }

    val path: Parser[List[CursorOp]] = cursorOp.rep.map(_.toList)
  }

  def from(raw: String): Either[Parser.Error, JsonPath] =
    if (raw === ".") JsonPath(Nil, raw).asRight
    else parsers.path.parseAll(raw).map(JsonPath(_, raw))

  implicit final val show: Show[JsonPath] = Show.show(_.raw)

  implicit final val decoder: Decoder[JsonPath] = Decoder[String].emap {
    from(_).leftMap(_.show)
  }
  implicit final val encoder: Encoder[JsonPath] = Encoder[String].contramap(_.raw)
}