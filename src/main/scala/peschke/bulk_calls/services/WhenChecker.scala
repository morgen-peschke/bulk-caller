package peschke.bulk_calls
package services

import models.When

import cats.Monad
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.functor._
import cats.syntax.traverse._
import io.circe.Json

trait WhenChecker[F[_]] {
  /**
    * Evaluate the [[peschke.bulk_calls.models.When]] condition against a [[io.circe.Json]] value
    *
    * See [[peschke.bulk_calls.models.When]] for the contract
    */
  def check(when: When, value: Json): F[Boolean]

  /**
    * Coerces a [[scala.Predef.String]] into either a JSON string or number, depending on what's supported.
    *
    * Otherwise identical to [[WhenChecker#check(peschke.bulk_calls.models.When, io.circe.Json)]]
    */
  def check(when: When, value: String): F[Boolean]
}
object WhenChecker {

  object syntax {
    implicit final class WhenCheckerOps(private val when: When) extends AnyVal {
      def check[F[_]](value: Json)(implicit WC: WhenChecker[F]): F[Boolean] = WC.check(when, value)

      def check[F[_]](value: String)(implicit WC: WhenChecker[F]): F[Boolean] = WC.check(when, value)
    }
  }

  def default[F[_]: Monad]: WhenChecker[F] = new Default[F]

  final class Default[F[_]: Monad] extends WhenChecker[F] with Serializable {
    override def check(when: When, value: String): F[Boolean] =
      when match {
        case When.LessThan(_) |
             When.LessThanOrEqual(_) |
             When.GreaterThan(_) |
             When.GreaterThanOrEqual(_) =>
          Either
            .catchOnly[NumberFormatException](BigDecimal(value))
            .toOption
            .map(Json.fromBigDecimal)
            .traverse(check(when, _))
            .map(_.getOrElse(false))
        case _ => check(when, Json.fromString(value))
      }

    override def check(when: When, value: Json): F[Boolean] =
      when match {
        case When.Not(nested) => check(nested, value).map(!_)
        case When.Equal(expected) => (value === expected).pure[F]
        case When.Exists(any) => any.traverse(check(_, value)).map(_.reduceLeft(_ || _))
        case When.Forall(all) => all.traverse(check(_, value)).map(_.reduceLeft(_ && _))
        case When.LessThan(expected) => value.as[BigDecimal].exists(_ < expected).pure[F]
        case When.LessThanOrEqual(expected) => value.as[BigDecimal].exists(_ <= expected).pure[F]
        case When.GreaterThan(expected) => value.as[BigDecimal].exists(_ > expected).pure[F]
        case When.GreaterThanOrEqual(expected) => value.as[BigDecimal].exists(_ >= expected).pure[F]
        case When.At(path, subWhen) => path.extract(value).toOption.flatten.fold(false.pure[F])(check(subWhen, _))
        case When.Any(subWhen) =>
          value.asArray
            .orElse(value.asObject.map(_.values.toVector))
            .getOrElse(Vector.empty[Json])
            .traverse(check(subWhen, _))
            .map(_.foldLeft(false)(_ || _))
        case When.StartsWith(prefix) => value.as[String].exists(_.startsWith(prefix)).pure[F]
        case When.EndsWith(suffix) => value.as[String].exists(_.endsWith(suffix)).pure[F]
        case When.Contains(substring) => value.as[String].exists(_.contains(substring)).pure[F]
      }
  }
}
