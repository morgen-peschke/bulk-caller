package peschke.bulk_calls
package models

import cats.Monoid
import io.circe.{Encoder, Json}

/**
  * Works around issues related to the automatic insertion of `Unit`
  */
sealed trait Done {
  def upcast: Done = this
}
object Done extends Done {
  implicit final val encoder: Encoder[Done] =
    Encoder.instance(_ => Json.fromString("done"))

  implicit final val monoid: Monoid[Done] = Monoid.instance(Done, (_, _) => Done)
}