package peschke.bulk_calls
package models

import models.Template.Element
import utils.NewType
import cats.Show
import cats.data.{Chain, NonEmptyList}
import cats.kernel.Monoid
import cats.syntax.functor._
import cats.syntax.foldable._
import cats.syntax.applicative._
import io.circe.syntax._
import io.circe.generic.semiauto._
import io.circe.{Codec, Decoder, Encoder, KeyDecoder}

import scala.annotation.{nowarn, tailrec}

/**
  * A template that can be expanded using a [[peschke.bulk_calls.services.TemplateExpander]]
  */
final case class Template(elements: NonEmptyList[Element]) {
  override def toString: String = Show[Template].show(this)
}

object Template {
  // Disable directly creating a Template
  @nowarn
  private def apply(elements: NonEmptyList[Element]): Template = new Template(elements)

  final val empty: Template = new Template(Element.Const("").pure[NonEmptyList])

  /**
    * This should be the default way to generate a [[Template]], as it normalizes the representation.
    */
  def fromList(elements: List[Element]): Template = {
    @tailrec
    def condense(unprocessed: List[Element],
                 currentConst: Chain[String],
                 accum: Chain[Element]): List[Element] = {
      def foldInCurrentConst = {
        val trailingConst = currentConst.fold
        if (trailingConst.isEmpty) accum
        else accum.append(Element.Const(trailingConst))
      }
      unprocessed match {
        case Nil => foldInCurrentConst.toList
        case (sub @ Element.Substitution(_)) :: rest =>
          condense(rest, Chain.empty, foldInCurrentConst.append(sub))
        case Element.Const(const) :: rest =>
          condense(rest, currentConst.append(const), accum)
      }
    }

    NonEmptyList.fromList(condense(elements, Chain.empty, Chain.empty)).fold(Template.empty)(new Template(_))
  }

  object Name extends NewType[String] {
    implicit final val keyDecoder: KeyDecoder[Type] = KeyDecoder[String].map(apply(_))
  }

  type Name = Name.Type

  sealed abstract class Element extends Product with Serializable {
    def upcast: Element = this
  }

  object Element {
    final case class Const(const: String) extends Element

    object Const {
      implicit final val codec: Codec[Const] = deriveCodec[Const]
    }

    final case class Substitution(sub: Name) extends Element

    object Substitution {
      implicit final val codec: Codec[Substitution] = deriveCodec[Substitution]
    }

    implicit final val codec: Codec[Element] = Codec.from(
      List[Decoder[Element]](
        Decoder[Const].widen,
        Decoder[Substitution].widen
      ).reduce(_ or _),
      Encoder.instance {
        case e@Const(_) => e.asJson
        case e@Substitution(_) => e.asJson
      }
    )
    implicit final val show: Show[Element] = Show.usingJsonEncoder[Element]
  }

  implicit final val codec: Codec[Template] = deriveCodec[Template]
  implicit final val show: Show[Template] = Show.usingJsonEncoder[Template]
  implicit final val monoid: Monoid[Template] =
    Monoid.instance(Template.empty, (a, b) => Template.fromList(a.elements.concatNel(b.elements).toList))
}
