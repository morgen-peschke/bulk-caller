package peschke.bulk_calls.utils

import io.circe.{Codec, Decoder, Encoder, Json}
import io.circe.generic.semiauto._
import io.circe.syntax._
import cats.syntax.either._
import org.scalacheck.{Arbitrary, Gen}
import peschke.bulk_calls.PropertyTest
import peschke.bulk_calls.utils.CirceUtilsTest.Foo
import peschke.bulk_calls.utils.CirceUtils._

class CirceUtilsTest extends PropertyTest {

  property("Encoder#withIdentifier should add an identification wrapper") {
    val wrappedEncoder = Foo.defaultEncoder.withIdentifier("foo")
    forAll { (foo: Foo) =>
      wrappedEncoder(foo) must === {
        Json.obj(
          CirceUtils.IdentifierKey -> "foo".asJson,
          CirceUtils.ValueKey -> Foo.defaultEncoder(foo)
        )
      }
    }
  }

  property("Decoder#withIdentifier should undo the results of Encoder#withIdentifier") {
    val wrappedEncoder = Foo.defaultEncoder.withIdentifier("foo")
    val wrappedDecoder = Foo.defaultDecoder.withIdentifier("foo")
    forAll { (foo: Foo) =>
      wrappedDecoder.decodeJson(wrappedEncoder(foo)).value must === (foo)
    }
  }

  property("Encoder#withIdentifier should produce an invisible round-trip") {
    val wrappedCodec = Foo.defaultCodec.withIdentifier("foo")
    forAll { (foo: Foo) =>
      wrappedCodec.decodeJson(wrappedCodec(foo)).value must === (foo)
    }
  }

  property("Encoder.either should encode both sides to a single value") {
    val encoder: Encoder[Either[Int, String]] = Encoder.either[Int, String]
    forAll(Gen.chooseNum(0, 100)) { input =>
      encoder(input.asLeft[String]) must === (Json.fromInt(input))
    }
    forAll(Gen.alphaStr) { input =>
      encoder(input.asRight[Int]) must === (Json.fromString(input))
    }
  }

  property("Decoder.either should decode both sides from a single value") {
    val decoder: Decoder[Either[Int, String]] = Decoder.either[Int, String]
    forAll(Gen.chooseNum(0, 100)) { input =>
      decoder.decodeJson(Json.fromInt(input)).value must === (input.asLeft[String])
    }
    forAll(Gen.alphaStr) { input =>
      decoder.decodeJson(Json.fromString(input)).value must === (input.asRight[Int])
    }
  }

  property("Decoder.either should bias right if both sides have the same type") {
    val decoder: Decoder[Either[Int, Int]] = Decoder.either[Int, Int]
    forAll(Gen.chooseNum(0, 100)) { input =>
      decoder.decodeJson(Json.fromInt(input)).value must === (input.asRight[Int])
    }
  }
}
object CirceUtilsTest {
  final case class Foo(a: Int, b: Char)
  object Foo {
    val gen: Gen[Foo] = for {
      a <- Arbitrary.arbitrary[Int]
      b <- Arbitrary.arbitrary[Char]
    } yield Foo(a,b)

    implicit val arb: Arbitrary[Foo] = Arbitrary(gen)

    final val defaultDecoder: Decoder[Foo] = deriveDecoder[Foo]
    final val defaultEncoder: Encoder[Foo] = deriveEncoder[Foo]
    final val defaultCodec: Codec[Foo] = Codec.from(defaultDecoder, defaultEncoder)
  }
}
