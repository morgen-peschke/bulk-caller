package peschke.bulk_calls
package models

import cats.Show
import cats.kernel.Semigroup
import cats.parse.{Numbers, Parser}
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.show._
import io.circe.Decoder.Result
import io.circe.{CursorOp, Decoder, Encoder, Json}

/**
  * Represents a path into a JSON graph
  *
  * @param segments each segment is a a movement one step deeper into the JSON graph
  *                 These may be:
  *                   - an identifier-style field, like `.foo`
  *                   - an index-style field, like `["foo"]`
  *                   - an array index, like `[1]`
  * @param raw the raw versions, saved to make it easy to make serialization stable.
  */
final case class JsonPath(segments: List[CursorOp], raw: String) {
  def extract(json: Json): Result[Option[Json]]g =
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
      Parser.char('.') *> (initial *> rest.rep0).string.withContext("method-style identifier")
    }

    val explicitField: Parser[String] =
      Parser.until(quote).surroundedBy(quote).withContext("index-style field")

    val arrayIndex: Parser[String] = Numbers.nonNegativeIntString.withContext("array index")

    val cursorOp: Parser[CursorOp] = {
      val downFieldSimple: Parser[CursorOp] = identifier.map(CursorOp.DownField).withContext("field")
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

  implicit final val semigroup: Semigroup[JsonPath] = Semigroup.instance { (lhs, rhs) =>
    JsonPath(
      lhs.segments ::: rhs.segments,
      (lhs.raw, rhs.raw) match {
        case (".", r) => r
        case (l, ".") => l
        case (l, r) => s"$l$r"
      }
    )
  }

  implicit final val decoder: Decoder[JsonPath] = Decoder[String].emap {
    from(_).leftMap(_.show)
  }
  implicit final val encoder: Encoder[JsonPath] = Encoder[String].contramap(_.raw)
}