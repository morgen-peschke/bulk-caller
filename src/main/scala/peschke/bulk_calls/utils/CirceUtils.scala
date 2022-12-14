package peschke.bulk_calls
package utils

import models.Done
import cats.syntax.apply._
import cats.syntax.either._
import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.syntax._
import peschke.bulk_calls.utils.CirceUtils.{CodecExtraOps, DecoderExtraOps, DecoderObjExtraOps, EncoderExtraOps, EncoderObjExtraOps, JsonOps}

import scala.reflect.runtime.universe.WeakTypeTag

trait CirceUtils {
  implicit def enableJsonOps(json: Json): JsonOps = new JsonOps(json)
  implicit def enableDecoderExtraOps[A](decoder: Decoder[A]): DecoderExtraOps[A] = new DecoderExtraOps(decoder)
  implicit def enableDecoderObjExtraOps(decoder: Decoder.type): DecoderObjExtraOps = new DecoderObjExtraOps(decoder)
  implicit def enableEncoderExtraOps[A](encoder: Encoder[A]): EncoderExtraOps[A] = new EncoderExtraOps(encoder)
  implicit def enableEncoderObjExtraOps(encoder: Encoder.type): EncoderObjExtraOps = new EncoderObjExtraOps(encoder)
  implicit def enableCodecExtraOps[A](codec: Codec[A]): CodecExtraOps[A] = new CodecExtraOps(codec)
}
object CirceUtils extends LazyLogging {
  private [utils] final val IdentifierKey = "type"
  private [utils] final val ValueKey = "value"

  private def singletonDecoder[V: Encoder](value: V): Decoder[Done] = {
    val expected = value.asJson
    val errorMsg = s"expected $IdentifierKey to be ${expected.compact}".asLeft
    val success = Done.asRight
    Decoder.instance(_.as[Json])
      .emap { actual =>
        if (actual == expected) success else errorMsg
      }
  }

  final class JsonOps(private val json: Json) extends AnyVal {
    /**
      * Handy alias for printing without spaces.
      */
    def compact: String = json.printWith(Printer.noSpaces)
  }

  final class DecoderExtraOps[A](private val decoder: Decoder[A]) extends AnyVal {
    /**
      * Remove the wrapper added by [[peschke.bulk_calls.utils.CirceUtils.EncoderExtraOps#withIdentifier]]
      */
    def withIdentifier(implicit tt: WeakTypeTag[A]): Decoder[A] =
      withIdentifier(s"${tt.tpe}")


    /**
      * Remove the wrapper added by [[peschke.bulk_calls.utils.CirceUtils.EncoderExtraOps#withIdentifier]]
      */
    def withIdentifier[V: Encoder](value: V): Decoder[A] = {
      val guard = singletonDecoder(value)
      Decoder.instance { hcursor =>
        hcursor.downField(IdentifierKey).as[Done](guard).productR {
          hcursor.downField(ValueKey).as[A](decoder)
        }
      }
    }
  }

  final class DecoderObjExtraOps(private val decoder: Decoder.type) extends AnyVal {
    /**
      * Decode both sides of an [[scala.util.Either]] from the same [[io.circe.Json]] value
      *
      * Warning: if both sides have the same type, it will always produce a [[scala.util.Right]]
      */
    def either[A, B](implicit aDecoder: Decoder[A], bDecoder: Decoder[B]): Decoder[Either[A, B]] =
      bDecoder.map(_.asRight).or(aDecoder.map(_.asLeft))
  }

  final class EncoderExtraOps[A](private val encoder: Encoder[A]) extends AnyVal {
    /**
      * Add a wrapper with a type-derived identifier.
      *
      * This can be very helpful when encoding-decoding ADTs
      */
    def withIdentifier(implicit tt: WeakTypeTag[A]): Encoder[A] =
      withIdentifier(s"${tt.tpe}")

    /**
      * Add a wrapper with an custom identifier
      *
      * This can be very helpful when encoding ADTs
      */
    def withIdentifier[V: Encoder](value: V): Encoder[A] = {
      val expected = value.asJson
      encoder.mapJson { result =>
        Json.obj(IdentifierKey -> expected, ValueKey -> result)
      }
    }
  }

  final class EncoderObjExtraOps(private val encoder: Encoder.type) extends AnyVal {
    /**
      * Encode both sides of an [[scala.util.Either]] to the same [[io.circe.Json]] value
      */
    def either[A: Encoder, B: Encoder]: Encoder[Either[A, B]] =
      encoder.instance(_.fold(_.asJson, _.asJson))
  }

  final class CodecExtraOps[A](private val codec: Codec[A]) extends AnyVal {
    /**
      * Bundles both [[EncoderExtraOps#withIdentifier]] and [[DecoderExtraOps#withIdentifier]]
      *
      * This can be handy to avoid typos in the identifier
      */
    def withIdentifier(implicit tt: WeakTypeTag[A]): Codec[A] =
      withIdentifier(s"${tt.tpe}")

    /**
      * Bundles both [[EncoderExtraOps#withIdentifier]] and [[DecoderExtraOps#withIdentifier]]
      *
      * This can be handy to avoid typos in the identifier
      */
    def withIdentifier[V: Encoder](value: V): Codec[A] =
      Codec.from(
        (codec: Decoder[A]).withIdentifier(value),
        (codec: Encoder[A]).withIdentifier(value)
      )
  }
}
