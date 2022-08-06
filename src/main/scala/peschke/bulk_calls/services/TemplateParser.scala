package peschke.bulk_calls
package services

import config.AppConfig
import config.TemplateConfig.SubstitutionMarkers
import models.Template
import models.Template.Element
import services.TemplateParser.InvalidTemplate
import cats.data.{EitherNel, NonEmptyList}
import cats.parse.{Parser, Parser0}
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.show._
import cats.{Applicative, Show}

trait TemplateParser[F[_]] {

  /**
   * Parse a [[models.Template]] from a [[scala.Predef.String]]
   */
  def parse(raw: String): F[EitherNel[InvalidTemplate,Template]]
}
object TemplateParser {
  def apply[F[_]](implicit TP: TemplateParser[F]): TP.type = TP

  sealed abstract class InvalidTemplate extends Product with Serializable {
    override def toString: String = Show[InvalidTemplate].show(this)
  }
  object InvalidTemplate {
    def apply(markers: SubstitutionMarkers, error: Parser.Error): InvalidTemplate =
      error.expected.head.context match {
        case List(Context.Close) => MissingCloseBrace(markers.close, error)
        case List(Context.Name) => MissingName(error)
        case _ => ParseFailure(error)
      }

    final case class MissingName(error: Parser.Error) extends InvalidTemplate
    final case class MissingCloseBrace(expected: String, error: Parser.Error) extends InvalidTemplate
    final case class ParseFailure(error: Parser.Error) extends InvalidTemplate

    implicit final val show: Show[InvalidTemplate] = Show.show {
      case MissingCloseBrace(expected, error) => show"Missing close brace <<$expected>>: $error"
      case MissingName(error) => show"Missing name after open brace: $error"
      case ParseFailure(error) => error.show
    }
  }

  /**
   * A micro typeclass to make it easier to work with [[supertagged.NewType]]
   *
   * Turns out we wrap a lot of raw templates in NewTypes
   */
  trait IsRawTemplate[A] {
    def retrieve(a: A): String
  }
  object IsRawTemplate {
    def instance[A](r: A => String): IsRawTemplate[A] = r(_)
  }

  object syntax {
    implicit final class TemplateParserOps[R](private val raw: R) extends AnyVal {
      def parse[F[_]](implicit TP: TemplateParser[F], RT: IsRawTemplate[R]): F[EitherNel[InvalidTemplate, Template]] =
        TP.parse(RT.retrieve(raw))
    }
  }

  def default[F[_]](implicit A: Applicative[F], appConfig: AppConfig): TemplateParser[F] =
    new Default[F](appConfig.templateConfig.substitutionMarkers)

  object Context {
    final val Open: String = "open"
    final val Close: String = "close"
    final val Const: String = "const"
    final val Name: String = "name"
  }

  final class Default[F[_]: Applicative](substitutionMarkers: SubstitutionMarkers)
    extends TemplateParser[F] with Serializable {

    private val open = Parser.string(substitutionMarkers.open).withContext(Context.Open)
    private val close = Parser.string(substitutionMarkers.close).withContext(Context.Close)

    private val const: Parser[Element] =
      Parser.until(open)
        .withContext(Context.Const)
        .map(Template.Element.Const(_))

    private val substitution: Parser[Element] =
      (open *> Parser.until(close).withContext(Context.Name) <* close)
        .map(Template.Name(_))
        .map(Template.Element.Substitution(_))

    private val initial: Parser[Element] = substitution | const
    private val rest: Parser[NonEmptyList[Element]] = (substitution | const).rep

    private val elements: Parser0[List[Element]] = (initial ~ rest.?).map {
      case (element, restOpt) => element :: restOpt.map(_.toList).getOrElse(Nil)
    }.?.map(_.getOrElse(Nil))

    override def parse(raw: String): F[EitherNel[InvalidTemplate,Template]] =
      elements.parseAll(raw).bimap(
        InvalidTemplate(substitutionMarkers, _).pure[NonEmptyList],
        Template.fromList
      ).pure[F]
  }
}
