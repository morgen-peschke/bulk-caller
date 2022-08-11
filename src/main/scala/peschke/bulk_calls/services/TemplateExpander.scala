package peschke.bulk_calls
package services

import config.{AppConfig, TemplateConfig}
import models.Template.{Element, Name}
import models.{Data, Template}
import services.TemplateExpander.ExpansionError
import services.TemplateExpander.ExpansionError.{ExpansionProducedInvalidJson, JsonArrayForbidden, JsonObjectForbidden, MissingSubstitution}
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

/**
  * Expand a [[peschke.bulk_calls.models.Template]]
  */
trait TemplateExpander[F[_]] {
  /**
    * Expands templates, with the restriction that a value may only be a scalar JSON value.
    */
  def expandText(template: Template, data: Data): F[ValidatedNel[ExpansionError, String]]

  /**
    * Expands a template, with the restriction that a value may only be a scalar JSON value
    * or a JSON array.
    *
    * Arrays are resolved by producing the product of the results.
    *
    * Example:
    * - template: `"{{a}}-{{b}}-{{c}}-{{d}}"`
    * - data: {{{
    *   {"a": [1,2], "b": true, "c": ['a'], "d": [1,2,3]}
    * }}}
    *
    * Expansions:
    * {{{
    *   1-true-a-1
    *   1-true-a-2
    *   1-true-a-3
    *   2-true-a-1
    *   2-true-a-2
    *   2-true-a-3
    * }}}
    */
  def expandTextWithRepetition(template: Template, data: Data): F[ValidatedNel[ExpansionError, List[String]]]

  /**
    * Expand a template by inlining the JSON values.
    *
    * Must produce a valid JSON string, which will be returned as a [[io.circe.Json]]
    */
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

    final case class JsonArrayForbidden(name: Name) extends ExpansionError

    object JsonArrayForbidden {
      implicit final val codec: Codec[JsonArrayForbidden] = deriveCodec[JsonArrayForbidden].withIdentifier
    }

    final case class JsonObjectForbidden(name: Name) extends ExpansionError

    object JsonObjectForbidden {
      implicit final val codec: Codec[JsonObjectForbidden] = deriveCodec[JsonObjectForbidden].withIdentifier
    }

    final case class ExpansionProducedInvalidJson(expanded: String,
                                                  parsingFailure: String) extends ExpansionError

    object ExpansionProducedInvalidJson {
      implicit final val codec: Codec[ExpansionProducedInvalidJson] =
        deriveCodec[ExpansionProducedInvalidJson].withIdentifier
    }


    implicit final val show: Show[ExpansionError] = Show.show {
      case MissingSubstitution(name) => show"missing $name"
      case JsonArrayForbidden(name) => show"$name cannot be a JSON array"
      case JsonObjectForbidden(name) => show"$name cannot be a JSON object"
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
        Decoder[JsonArrayForbidden].widen,
        Decoder[JsonObjectForbidden].widen,
        Decoder[ExpansionProducedInvalidJson].widen
      ).reduceLeft(_ or _),
      Encoder.instance {
        case ee@MissingSubstitution(_) => ee.asJson
        case ee@JsonArrayForbidden(_) => ee.asJson
        case ee@JsonObjectForbidden(_) => ee.asJson
        case ee@ExpansionProducedInvalidJson(_, _) => ee.asJson
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

  def default[F[_] : Applicative](implicit appConfig: AppConfig): TemplateExpander[F] =
    new Default[F](appConfig.constants, appConfig.templateConfig)

  final class Default[F[_] : Applicative](constants: Data.Constants, config: TemplateConfig)
    extends TemplateExpander[F] with Serializable {

    def stringifyJson(name: Name, json: Json): ValidatedNel[ExpansionError, String] =
      json.fold(
        "null".validNel[ExpansionError],
        b => (if (b) "true" else "false").validNel,
        n => {
          val bigD = n.toBigDecimal.getOrElse(BigDecimal.valueOf(n.toDouble))
          if (config.doNotUseExponents) bigD.bigDecimal.toPlainString.validNel
          else bigD.bigDecimal.toEngineeringString.validNel
        },
        _.validNel,
        _ => JsonArrayForbidden(name).upcast.invalidNel,
        _ => JsonObjectForbidden(name).upcast.invalidNel
      )

    def stringifyJsonWithReplacements(name: Name, json: Json): ValidatedNel[ExpansionError, Chain[String]] =
      json.asArray.fold(stringifyJson(name, json).map(Chain.one)) {
        Chain.fromSeq(_).traverse(stringifyJson(name, _))
      }

    def combineChunks(chunks: List[Chain[String]]): List[String] = {
      def loop(remaining: List[Chain[String]]): Chain[Chain[String]] =
        remaining match {
          case Nil => Chain.one(Chain.empty)
          case currentGroup :: rest =>
            loop(rest).flatMap { result =>
              currentGroup.map { chunk =>
                result.prepend(chunk)
              }
            }
        }

      loop(chunks).map(_.mkString_("")).toList
    }

    override def expandText(template: Template, data: Data): F[ValidatedNel[ExpansionError, String]] =
      template.elements
        .traverse {
          case Element.Const(value) => value.valid
          case Element.Substitution(name) =>
            data.values.get(name).orElse(constants.values.get(name)) match {
              case None if config.allowEmpty => config.placeholders.text.valid
              case None => MissingSubstitution(name).upcast.invalidNel
              case Some(value) => stringifyJson(name, value)
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
              case Some(value) => stringifyJsonWithReplacements(name, value)
            }
        }
        .map(expansions => combineChunks(expansions.toList))
        .pure[F]

    override def expandJson(template: Template, data: Data): F[ValidatedNel[ExpansionError, Json]] =
      template.elements
        .traverse {
          case Element.Const(value) => value.valid
          case Element.Substitution(name) =>
            data.values.get(name).orElse(constants.values.get(name)) match {
              case None if config.allowEmpty => config.placeholders.jsonText.valid
              case None => MissingSubstitution(name).upcast.invalidNel
              case Some(value) => value.compact.valid
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