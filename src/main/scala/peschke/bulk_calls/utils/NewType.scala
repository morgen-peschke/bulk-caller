package peschke.bulk_calls
package utils

import cats.Show
import cats.syntax.show._
import io.circe.{Codec, Decoder, Encoder}

/**
  * Wrapper around [[supertagged.NewType]] that forwards a set of common instances.
  */
abstract class NewType[T: Encoder: Decoder: Show] extends supertagged.NewType[T]{
  implicit final val codec: Codec[Type] = Codec.from(
    Decoder[T].map(apply(_)),
    Encoder[T].contramap(raw)
  )
  implicit final val show: Show[Type] = Show.show(raw(_).show)
}