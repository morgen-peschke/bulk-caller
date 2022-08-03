package peschke.bulk_calls
package services

import config.AppConfig
import config.TemplateConfig.SubstitutionMarkers
import models.Template
import models.Template.Element
import services.TemplateParser.InvalidTemplate

import cats.data.{EitherNel, NonEmptyList}
import cats.parse.Parser
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.show._
import cats.{Applicative, Show}

trait TemplateParser[F[_]] {
  def parse(raw: String): F[EitherNel[InvalidTemplate,Template]]
}
object TemplateParser {
  def apply[F[_]](implicit TP: TemplateParser[F]): TP.type = TP

  object InvalidTemplate extends supertagged.NewType[Parser.Error] {
    implicit final val show: Show[Type] = Show.show(raw(_).show)
  }
  type InvalidTemplate = InvalidTemplate.Type

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

  final class Default[F[_]: Applicative](substitutionMarkers: SubstitutionMarkers)
    extends TemplateParser[F] with Serializable {

    private val open = Parser.string(substitutionMarkers.open)
    private val close = Parser.string(substitutionMarkers.close)

    private val const: Parser[Element] = Parser.until(open).map(Template.Element.Const)
    private val substitution: Parser[Element] =
      Parser.until(close).map(Template.Name(_)).map(Template.Element.Substitution)

    private val initial: Parser[Element] = (open *> substitution) | const
    private val rest: Parser[NonEmptyList[Element]] = (const | substitution).rep

    private val parser: Parser[Template] = (initial ~ rest.?).map {
      case (element, restOpt) =>
        Template(NonEmptyList(element, restOpt.map(_.toList).getOrElse(Nil)))
    }

    override def parse(raw: String): F[EitherNel[InvalidTemplate,Template]] =
      parser.parseAll(raw).leftMap(InvalidTemplate(_).pure[NonEmptyList]).pure[F]
  }
}
