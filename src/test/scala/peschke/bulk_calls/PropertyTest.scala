package peschke.bulk_calls

import cats.data.NonEmptyList
import cats.syntax.eq._
import cats.syntax.traverse._
import io.circe.Json
import io.circe.syntax._
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalacheck.cats.instances._
import org.scalatest.{EitherValues, OptionValues}
import org.scalatest.matchers.must.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import peschke.bulk_calls.PropertyTest.standardGens
import peschke.bulk_calls.models.Template
import peschke.bulk_calls.models.Template.Element.Substitution

import java.math.{MathContext, RoundingMode}

trait CommonSyntax extends Matchers with EitherValues with OptionValues with ScalaCheckDrivenPropertyChecks {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
}

trait PropertyTest extends AnyPropSpec with CommonSyntax
object PropertyTest {
  object syntax {
    implicit final class RangeToGenOps(private val r: Range) extends AnyVal {
      def gen: Gen[Int] = Gen.chooseNum(r.start, r.end).map(i => i - (i % r.step))
    }

    implicit final class GenOps[A](private val gen: Gen[A]) extends AnyVal {
      def list(r: Range): Gen[List[A]] = r.gen.flatMap(Gen.listOfN(_, gen))

      def nel(r: Range): Gen[NonEmptyList[A]] =
        for {
          head <- gen
          tail <- list(r.start + r.step to r.last by r.step)
        } yield NonEmptyList(head, tail)
    }

    implicit final class CharGenOps(private val gen: Gen[Char]) extends AnyVal {
      def string(r: Range): Gen[String] = r.gen.flatMap(Gen.stringOfN(_, gen))
    }
  }

  import syntax._

  val genBigNumbers: Gen[BigDecimal] = {
    val mathContexts = (1 to 5).gen.map(new MathContext(_, RoundingMode.DOWN))
    val base = BigDecimal(Long.MaxValue)
    for {
      mathContext <- mathContexts
      raw <- Gen.chooseNum(base, base * 2)
    } yield raw(mathContext).rounded
  }

  object jsonGens {
    val nullGen: Gen[Json] = Gen.const(Json.Null)
    val boolGen: Gen[Json] = Arbitrary.arbitrary[Boolean].map(_.asJson)
    val stringGen: Gen[(String, Json)] = Arbitrary.arbitrary[String].map(s => s -> s.asJson)
    val numberGen: Gen[(Long, Json)] =
      Gen.chooseNum(-1000L, 1000L).map(bd => bd -> bd.asJson)

    def arrayGen(count: Range, values: Gen[Json]): Gen[Json] = values.list(count).map(_.asJson)

    val fieldNames: Gen[String] = Gen.alphaNumChar.string(1 to 30)

    def objGen(count: Range, values: Gen[Json]): Gen[Json] = {
      val fieldGen: Gen[(String, Json)] =
        for {
          field <- fieldNames
          value <- values
        } yield field -> value

      fieldGen.list(count).map(Json.fromFields)
    }

    def objGen(fields: (String,Gen[Json])*): Gen[Json] =
      fields.toList
        .traverse {
          case (name, valueGen) => valueGen.map(name -> _)
        }
        .map(Json.fromFields)

    val scalarGen: Gen[Json] = Gen.oneOf(stringGen.map(_._2), numberGen.map(_._2), boolGen, nullGen)

    def graph(objFieldCount: Range, arrayLength: Range, maxDepth: Range): Gen[Json] = {
      def subGen(depth: Int): Gen[Json] =
        if (depth <= 0) scalarGen
        else Gen.oneOf(scalarGen, recurseObj(depth - 1), recurseArr(depth - 1))

      def recurseArr(depth: Int): Gen[Json] = arrayGen(arrayLength, subGen(depth - 1))

      def recurseObj(depth: Int): Gen[Json] = objGen(objFieldCount, subGen(depth - 1))

      maxDepth.gen.flatMap(subGen)
    }

    def different(referenceGen: Gen[Json]): Gen[(Json, Json)] =
      referenceGen.flatMap { reference =>
        referenceGen.flatMap { attempt1 =>
          if (reference =!= attempt1) Gen.const(reference -> attempt1)
          else referenceGen.flatMap { attempt2 =>
            Gen.const {
              reference -> {
                if (reference =!= attempt2) attempt2
                else attempt2.fold[Json](
                  jsonNull = Json.fromString("not null"),
                  jsonBoolean = bool => Json.fromBoolean(!bool),
                  jsonNumber = n => Json.fromString(s"not $n"),
                  jsonString = s => Json.fromString(s"not $s"),
                  jsonArray = _ => Json.fromString("not array"),
                  jsonObject = _ => Json.fromString("not obj")
                )
              }
            }
          }
        }
      }
  }

  object standardGens {
    val templateNames: Gen[Template.Name] = Gen.alphaNumChar.string(1 to 10).map(Template.Name(_))

    val constants: Gen[Template.Element.Const] = Gen.alphaNumChar.string(1 to 10).map(Template.Element.Const(_))

    object substitutions {
      def string(valueGen: Gen[String]): Gen[(Substitution, String, Json)] =
        for {
          name <- templateNames
          (rendered, value) <- valueGen.map(s => s -> s.asJson)
        } yield (Substitution(name), rendered, value)

      def stringList(valueGen: Gen[List[String]]): Gen[(Substitution, List[String], Json)] =
        for {
          name <- templateNames
          (rendered, value) <- valueGen.map(s => s -> s.asJson)
        } yield (Substitution(name), rendered, value)

      def renderedJson(valueGen: Gen[(String, Json)]): Gen[(Substitution, String, Json)] =
        for {
          name <- templateNames
          (rendered, value) <- valueGen
        } yield (Substitution(name), rendered, value)

      def renderedJsonList(valueGen: Gen[(List[String], Json)]): Gen[(Substitution, List[String], Json)] =
        for {
          name <- templateNames
          (rendered, value) <- valueGen
        } yield (Substitution(name), rendered, value)
    }
  }
}
trait DefaultTestInstances {
  implicit val arbTemplateName: Arbitrary[Template.Name] = Arbitrary(standardGens.templateNames)
  implicit val arbConst: Arbitrary[Template.Element.Const] = Arbitrary(standardGens.constants)
}

trait WordTest extends AnyWordSpec with CommonSyntax