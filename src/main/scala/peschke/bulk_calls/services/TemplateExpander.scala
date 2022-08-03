package peschke.bulk_calls
package services

import config.{AppConfig, TemplateConfig}
import models.Template.{Element, Name}
import models.{Data, Template}
import services.TemplateExpander.ExpansionError
import services.TemplateExpander.ExpansionError.{ExpansionProducedInvalidJson, MissingSubstitution, ScalarRequired}
import utils.CirceUtils._

import cats.data.{Chain, ValidatedNel}
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.show._
import cats.syntax.traverse._
import cats.syntax.validated._
import cats.{Applicative, Show}
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._

import scala.annotation.tailrec

trait TemplateExpander[F[_]] {
  def expandText(template: Template, data: Data): F[ValidatedNel[ExpansionError, String]]

  def expandTextWithRepetition(template: Template, data: Data): F[ValidatedNel[ExpansionError, List[String]]]

  def expandJson(template: Template, data: Data): F[ValidatedNel[ExpansionError, Json]]
}
object TemplateExpander {

  sealed abstract class ExpansionError extends Product with Serializable {
    def upcast: ExpansionError = this
  }
  object ExpansionError {
    final case class MissingSubstitution(name: Name) extends ExpansionError
    object MissingSubstitution {
      implicit final val codec: Codec[MissingSubstitution] = deriveCodec[MissingSubstitution].withIdentifier
    }

    final case class ScalarRequired(name: Name) extends ExpansionError
    object ScalarRequired {
      implicit final val codec: Codec[ScalarRequired] = deriveCodec[ScalarRequired].withIdentifier
    }

    final case class ExpansionProducedInvalidJson(expanded: String,
                                                  parsingFailure: String) extends ExpansionError

    object ExpansionProducedInvalidJson {
      implicit final val codec: Codec[ExpansionProducedInvalidJson] =
        deriveCodec[ExpansionProducedInvalidJson].withIdentifier
    }


    implicit final val show: Show[ExpansionError] = Show.show {
      case MissingSubstitution(name) => show"missing $name"
      case ScalarRequired(name) => show"$name cannot be a JSON object or array"
      case ExpansionProducedInvalidJson(expanded, parsingFailure) =>
        s"""|Expanded template produced invalid JSON
            |========== Expanded Template ==========
            |$expanded
            |========== Parsing Failure   ==========
            |$parsingFailure
            |=======================================
            |""".stripMargin
    }

    implicit final val codec: Codec[ExpansionError] = Codec.from(
      List[Decoder[ExpansionError]](
        Decoder[MissingSubstitution].widen,
        Decoder[ScalarRequired].widen,
        Decoder[ExpansionProducedInvalidJson].widen
      ).reduceLeft(_ or _),
      Encoder.instance {
        case ee @ MissingSubstitution(_) => ee.asJson
        case ee @ ScalarRequired(_) => ee.asJson
        case ee @ ExpansionProducedInvalidJson(_, _) => ee.asJson
      }
    )
  }

  object syntax {
    implicit final class TemplateExpanderOps(private val template: Template) extends AnyVal {
      def expandText[F[_]](data: Data)(implicit TE: TemplateExpander[F])
      : F[ValidatedNel[ExpansionError, String]] =
        TE.expandText(template, data)

      def expandTextWithRepetition[F[_]](data: Data)(implicit TE: TemplateExpander[F])
      : F[ValidatedNel[ExpansionError, List[String]]] =
        TE.expandTextWithRepetition(template, data)

      def expandJson[F[_]](data: Data)(implicit TE: TemplateExpander[F])
      : F[ValidatedNel[ExpansionError, Json]] =
        TE.expandJson(template, data)
    }
  }

  def default[F[_]: Applicative](implicit appConfig: AppConfig): TemplateExpander[F] =
    new Default[F](appConfig.constants, appConfig.templateConfig)

  final class Default[F[_]: Applicative](constants: Data.Constants, config: TemplateConfig)
    extends TemplateExpander[F] with Serializable {

    def stringifyJson(name: Name, json: Json): ValidatedNel[ExpansionError, Chain[String]] = {
      val nullAttempt = json.asNull.map(_ => Chain.one("null").valid)
      val numberAttempt = json.asNumber.flatMap(_.toBigDecimal).map { bigD =>
        Chain.one {
          if (config.doNotUseExponents) bigD.bigDecimal.toPlainString
          else bigD.bigDecimal.toEngineeringString
        }.valid
      }
      val stringAttempt = json.asString.map(Chain.one(_).valid)
      val booleanAttempt = json.asBoolean.map(b => Chain.one(if (b) "true" else "false").valid)
      val arrayAttempt = json.asArray.map { children =>
        children
          .traverse(stringifyJson(name, _))
          .andThen(_.traverse(_.uncons match {
            case Some((head, rest)) if rest.isEmpty => head.validNel[ExpansionError]
            case _ => ScalarRequired(name).upcast.invalidNel
          }).map(Chain.fromSeq(_)))
      }

      nullAttempt
        .orElse(numberAttempt)
        .orElse(stringAttempt)
        .orElse(booleanAttempt)
        .orElse(arrayAttempt)
        .getOrElse(ScalarRequired(name).upcast.invalidNel)
    }

    @tailrec
    def combineChunks(remaining: List[Chain[String]], accum: Chain[Chain[String]]): Chain[Chain[String]] =
      remaining match {
        case Nil => accum
        case currentGroup :: rest =>
          val updatedAccum: Chain[Chain[String]] = currentGroup.flatMap { chunk =>
            accum.map(_.append(chunk))
          }
          combineChunks(rest, updatedAccum)
      }

    override def expandText(template: Template, data: Data): F[ValidatedNel[ExpansionError, String]] =
      template.elements
        .traverse {
          case Element.Const(value) => value.valid
          case Element.Substitution(name) =>
            data.values.get(name).orElse(constants.values.get(name)) match {
              case None if config.allowEmpty => config.placeholders.text.valid
              case None => MissingSubstitution(name).upcast.invalidNel
              case Some(value) =>
                stringifyJson(name, value).andThen(_.uncons match {
                  case Some((str, rest)) if rest.isEmpty => str.valid
                  case _ => ScalarRequired(name).upcast.invalidNel
                })
            }
        }
        .map(_.toList.mkString)
        .pure[F]

    def expandTextWithRepetition(template: Template, data: Data): F[ValidatedNel[ExpansionError, List[String]]] =
      template
        .elements
        .traverse {
          case Element.Const(value) => Chain.one(value).valid
          case Element.Substitution(name) =>
            data.values.get(name).orElse(constants.values.get(name)) match {
              case None if config.allowEmpty => Chain.one(config.placeholders.text).valid
              case None => MissingSubstitution(name).upcast.invalidNel
              case Some(value) => stringifyJson(name, value)
            }
        }
        .map(expansions => combineChunks(expansions.toList, Chain.empty))
        .map(_.map(_.mkString_("")).toList)
        .pure[F]

    override def expandJson(template: Template, data: Data): F[ValidatedNel[ExpansionError, Json]] =
      template.elements
        .traverse {
          case Element.Const(value) => value.valid
          case Element.Substitution(name) =>
            data.values.get(name).orElse(constants.values.get(name)) match {
              case None if config.allowEmpty => config.placeholders.jsonText.valid
              case None => MissingSubstitution(name).upcast.invalidNel
              case Some(value) => value.printWith(Printer.noSpaces).valid
            }
        }
        .map(_.toList.mkString)
        .andThen { combinedText =>
          io.circe.parser
            .parse(combinedText)
            .leftMap(pf => ExpansionProducedInvalidJson(combinedText, pf.show))
            .toValidatedNel
        }
        .pure[F]
  }
}