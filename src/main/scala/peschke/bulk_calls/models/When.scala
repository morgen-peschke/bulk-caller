package peschke.bulk_calls
package models

import cats.data.NonEmptyList
import cats.syntax.functor._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}

/**
  * Defines conditions that can be used to accept or reject value.
  *
  * All of these can match a [[io.circe.Json]].
  *
  * Some of them can match a scalar value, by trying to coerce it into an appropriate value. At
  * the moment, this is restricted to a hard-coded lift for [[scala.Predef.String]].
  */
sealed abstract class When extends Product with Serializable
object When {
  /**
    * Negates the result of the nested [[When]]
    */
  final case class Not(nested: When) extends When
  object Not {
    implicit final val decoder: Decoder[Not] = Decoder.instance { hocon =>
      hocon.downField("not").as[When].map(Not(_))
    }
    implicit final val encoder: Encoder[Not] = Encoder.instance { op =>
      Json.obj("not" -> op.nested.asJson)
    }
  }

  /**
    * A strict equality check.
    *
    * Notably, no coercion applies here, so while this may match:
    * {{{
    *   WhenChecker[F].check(When.LessThan(5.asJson), "5")
    * }}}
    * This will not:
    * {{{
    *   WhenChecker[F].check(When.Equal(5.asJson), "5")
    * }}}
    */
  final case class Equal(expected: Json) extends When
  object Equal {
    implicit final val decoder: Decoder[Equal] = Decoder.instance { hocon =>
      hocon.downField("equal").as[Json].map(Equal(_))
    }
    implicit final val encoder: Encoder[Equal] = Encoder.instance { op =>
      Json.obj("equal" -> op.expected.asJson)
    }
  }

  /**
    * Accepts an input if at least one of the nested [[When]] accepts it
    */
  final case class Exists(any: NonEmptyList[When]) extends When
  object Exists {
    implicit final val decoder: Decoder[Exists] = Decoder.instance { hocon =>
      hocon.downField("exists").as[NonEmptyList[When]].map(Exists(_))
    }
    implicit final val encoder: Encoder[Exists] = Encoder.instance { op =>
      Json.obj("exists" -> op.any.asJson)
    }
  }

  /**
    * Accepts an input iif all of the nested [[When]] accept it
    */
  final case class Forall(all: NonEmptyList[When]) extends When
  object Forall {
    implicit final val decoder: Decoder[Forall] = Decoder.instance { hocon =>
      hocon.downField("forall").as[NonEmptyList[When]].map(Forall(_))
    }
    implicit final val encoder: Encoder[Forall] = Encoder.instance { op =>
      Json.obj("forall" -> op.all.asJson)
    }
  }

  /**
    * A standard `<` comparison
    *
    * This fails if the value is not [[io.circe.JsonNumber]], or a [[scala.Predef.String]] that can be
    * coerced to a number
    */
  final case class LessThan(expected: BigDecimal) extends When
  object LessThan {
    implicit final val decoder: Decoder[LessThan] = Decoder.instance { hocon =>
      hocon.downField("<").as[BigDecimal].map(LessThan(_))
    }
    implicit final val encoder: Encoder[LessThan] = Encoder.instance { op =>
      Json.obj("<" -> op.expected.asJson)
    }
  }

  /**
    * A standard `<=` comparison
    *
    * This fails if the value is not [[io.circe.JsonNumber]], or a [[scala.Predef.String]] that can be
    * coerced to a number
    */
  final case class LessThanOrEqual(expected: BigDecimal) extends When
  object LessThanOrEqual {
    implicit final val decoder: Decoder[LessThanOrEqual] = Decoder.instance { hocon =>
      hocon.downField("<=").as[BigDecimal].map(LessThanOrEqual(_))
    }
    implicit final val encoder: Encoder[LessThanOrEqual] = Encoder.instance { op =>
      Json.obj("<=" -> op.expected.asJson)
    }
  }

  /**
    * A standard `>` comparison
    *
    * This fails if the value is not [[io.circe.JsonNumber]], or a [[scala.Predef.String]] that can be
    * coerced to a number
    */
  final case class GreaterThan(expected: BigDecimal) extends When
  object GreaterThan {
    implicit final val decoder: Decoder[GreaterThan] = Decoder.instance { hocon =>
      hocon.downField(">").as[BigDecimal].map(GreaterThan(_))
    }
    implicit final val encoder: Encoder[GreaterThan] = Encoder.instance { op =>
      Json.obj(">" -> op.expected.asJson)
    }
  }

  /**
    * A standard `>=` comparison
    *
    * This fails if the value is not [[io.circe.JsonNumber]], or a [[scala.Predef.String]] that can be
    * coerced to a number
    */
  final case class GreaterThanOrEqual(expected: BigDecimal) extends When
  object GreaterThanOrEqual {
    implicit final val decoder: Decoder[GreaterThanOrEqual] = Decoder.instance { hocon =>
      hocon.downField("=>").as[BigDecimal].map(GreaterThanOrEqual(_))
    }
    implicit final val encoder: Encoder[GreaterThanOrEqual] = Encoder.instance { op =>
      Json.obj("=>" -> op.expected.asJson)
    }
  }

  /**
    * Allows comparing within a nested structure.
    *
    * @param path Used to extract the test value from the input.
    *             If this path does not exist, the input is rejected.
    * @param when Used to test the extracted value.
    */
  final case class At(path: JsonPath, when: When) extends When
  object At {
    implicit final val decoder: Decoder[At] = Decoder.instance { hocon =>
      for {
        path <- hocon.downField("at").downField("path").as[JsonPath]
        when <- hocon.downField("at").downField("when").as[When]
      } yield At(path, when)
    }
    implicit final val encoder: Encoder[At] = Encoder.instance { op =>
      Json.obj("at" -> Json.obj(
        "path" -> op.path.asJson,
        "when" -> op.when.asJson
      ))
    }
  }

  /**
    * Accepts if the nested [[When]] accepts any of the values inside the input.
    *
    * For JSON arrays, each element is tested.
    * For JSON objects, each field's value is tested (keys are ignored).
    * For all other JSON types, the value is rejected.
    */
  final case class Any(when: When) extends When
  object Any {
    implicit final val decoder: Decoder[Any] = Decoder.instance { hocon =>
      hocon.downField("any").as[When].map(Any(_))
    }
    implicit final val encoder: Encoder[Any] = Encoder.instance { op =>
      Json.obj("any" -> op.when.asJson)
    }
  }

  /**
    * Accepts JSON strings that start with `prefix`
    */
  final case class StartsWith(prefix: String) extends When
  object StartsWith {
    implicit final val decoder: Decoder[StartsWith] = Decoder.instance { hocon =>
      hocon.downField("starts-with").as[String].map(StartsWith(_))
    }
    implicit final val encoder: Encoder[StartsWith] = Encoder.instance { op =>
      Json.obj("starts-with" -> op.prefix.asJson)
    }
  }

  /**
    * Accepts JSON strings that end with `suffix`
    */
  final case class EndsWith(suffix: String) extends When
  object EndsWith {
    implicit final val decoder: Decoder[EndsWith] = Decoder.instance { hocon =>
      hocon.downField("ends-with").as[String].map(EndsWith(_))
    }
    implicit final val encoder: Encoder[EndsWith] = Encoder.instance { op =>
      Json.obj("ends-with" -> op.suffix.asJson)
    }
  }

  /**
    * Accepts JSON strings that contain `substring`
    */
  final case class Contains(substring: String) extends When
  object Contains {
    implicit final val decoder: Decoder[Contains] = Decoder.instance { hocon =>
      hocon.downField("contains").as[String].map(Contains(_))
    }
    implicit final val encoder: Encoder[Contains] = Encoder.instance { op =>
      Json.obj("contains" -> op.substring.asJson)
    }
  }

  implicit final val decoder: Decoder[When] =
    List[Decoder[When]](
      Decoder[Not].widen,
      Decoder[Equal].widen,
      Decoder[Exists].widen,
      Decoder[Forall].widen,
      Decoder[LessThan].widen,
      Decoder[LessThanOrEqual].widen,
      Decoder[GreaterThan].widen,
      Decoder[GreaterThanOrEqual].widen,
      Decoder[At].widen,
      Decoder[Any].widen,
      Decoder[StartsWith].widen,
      Decoder[EndsWith].widen,
      Decoder[Contains].widen
    ).reduce(_ or _)

  implicit final val encoder: Encoder[When] =
    Encoder.instance {
      case co@Not(_) => co.asJson
      case co@Equal(_) => co.asJson
      case co@Exists(_) => co.asJson
      case co@Forall(_) => co.asJson
      case co@LessThan(_) => co.asJson
      case co@LessThanOrEqual(_) => co.asJson
      case co@GreaterThan(_) => co.asJson
      case co@GreaterThanOrEqual(_) => co.asJson
      case co@At(_, _) => co.asJson
      case co@Any(_) => co.asJson
      case co@StartsWith(_) => co.asJson
      case co@EndsWith(_) => co.asJson
      case co@Contains(_) => co.asJson
    }
}