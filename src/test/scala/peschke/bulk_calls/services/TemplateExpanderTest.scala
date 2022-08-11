package peschke.bulk_calls
package services

import cats.Id
import cats.data.NonEmptyList
import cats.instances.order._
import cats.syntax.either._
import cats.syntax.show._
import cats.syntax.validated._
import io.circe.syntax._
import io.circe.{Json, parser}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.EitherValues
import org.scalatest.matchers.Matcher
import peschke.bulk_calls.PropertyTest.syntax._
import peschke.bulk_calls.PropertyTest.{genBigNumbers, jsonGens, standardGens}
import peschke.bulk_calls.config.TemplateConfig
import peschke.bulk_calls.config.TemplateConfig.{Placeholders, SubstitutionMarkers}
import peschke.bulk_calls.models.Data.Constants
import peschke.bulk_calls.models.Template.Element.{Const, Substitution}
import peschke.bulk_calls.models.Template.Name
import peschke.bulk_calls.models.{Data, Template}
import peschke.bulk_calls.services.TemplateExpander.ExpansionError
import peschke.bulk_calls.services.TemplateExpander.ExpansionError.{ExpansionProducedInvalidJson, JsonArrayForbidden, JsonObjectForbidden}
import peschke.bulk_calls.services.TemplateExpanderTest.{ExpandFailure, ExpandJsonSuccess, ExpandSuccess, ExpandTextSuccess, ExpandTextWithRepetitionSuccess}
import peschke.bulk_calls.{DefaultTestInstances, PropertyTest}

class TemplateExpanderTest extends PropertyTest {
  val expandText: Matcher[ExpandSuccess[String]] = Matcher { test =>
    val expander = new TemplateExpander.Default[Id](test.constants, test.config)
    be(test.expected.validNel[ExpansionError])(expander.expandText(test.input, test.data))
  }

  val expandTextWithRepetition: Matcher[ExpandSuccess[List[String]]] = Matcher { test =>
    val expander = new TemplateExpander.Default[Id](test.constants, test.config)
    be(test.expected.validNel[ExpansionError]) {
      expander.expandTextWithRepetition(test.input, test.data).map(_.sorted)
    }
  }

  val expandJson: Matcher[ExpandSuccess[Json]] = Matcher { test =>
    val expander = new TemplateExpander.Default[Id](test.constants, test.config)
    be(test.expected.validNel[ExpansionError])(expander.expandJson(test.input, test.data))
  }

  val refuseToExpandText: Matcher[ExpandFailure] = Matcher { test =>
    val expander = new TemplateExpander.Default[Id](test.constants, test.config)
    be(test.expected.invalid[String])(expander.expandText(test.input, test.data))
  }

  val refuseToExpandTextWithRepetition: Matcher[ExpandFailure] = Matcher { test =>
    val expander = new TemplateExpander.Default[Id](test.constants, test.config)
    be(test.expected.invalid[List[String]])(expander.expandTextWithRepetition(test.input, test.data))
  }

  val refuseToExpandJson: Matcher[ExpandFailure] = Matcher { test =>
    val expander = new TemplateExpander.Default[Id](test.constants, test.config)
    be(test.expected.invalid[Json])(expander.expandJson(test.input, test.data))
  }

  //region TemplateExpander.expandText

  property("TemplateExpander.expandText should properly handle missing values using placeholders") {
    forAll(ExpandTextSuccess.dataMissingValues)(_ must expandText)
  }

  property("TemplateExpander.expandText should properly handle missing values when forbidden") {
    forAll(ExpandFailure.dataMissingValues)(_ must refuseToExpandText)
  }

  property("TemplateExpander.expandText should properly expand a constant template") {
    forAll(ExpandTextSuccess.constantTemplates)(_ must expandText)
  }

  property("TemplateExpander.expandText should properly expand a substitution template") {
    forAll(ExpandTextSuccess.substitutionTemplates)(_ must expandText)
  }

  property("TemplateExpander.expandText should properly expand a mixed template") {
    forAll(ExpandTextSuccess.mixedTemplates)(_ must expandText)
  }

  property("TemplateExpander.expandText should properly fall back to constants") {
    forAll(ExpandTextSuccess.missingDataWithConstants)(_ must expandText)
  }

  property("TemplateExpander.expandText should properly render a non-string JSON scalar") {
    forAll(ExpandTextSuccess.dataWithNonStringScalars)(_ must expandText)
  }

  property("TemplateExpander.expandText should properly render a large JSON number") {
    forAll(ExpandTextSuccess.dataWithLargeNumbers)(_ must expandText)
  }

  property("TemplateExpander.expandText should fail if provided a non-scalar JSON value") {
    forAll(ExpandFailure.dataWithNonScalarJsonValues)(_ must refuseToExpandText)
  }

  //endregion

  //region TemplateExpander.expandTextWithRepetition

  property("TemplateExpander.expandTextWithRepetition should properly handle missing values using placeholders") {
    forAll(ExpandTextWithRepetitionSuccess.missingValues)(_ must expandTextWithRepetition)
  }

  property("TemplateExpander.expandTextWithRepetition should properly handle missing values when forbidden") {
    forAll(ExpandFailure.dataMissingValues)(_ must refuseToExpandTextWithRepetition)
  }

  property("TemplateExpander.expandTextWithRepetition should properly expand a constant template") {
    forAll(ExpandTextWithRepetitionSuccess.constantTemplates)(_ must expandTextWithRepetition)
  }

  property("TemplateExpander.expandTextWithRepetition should properly expand a substitution template") {
    forAll(ExpandTextWithRepetitionSuccess.substitutionTemplates)(_ must expandTextWithRepetition)
  }

  property("TemplateExpander.expandTextWithRepetition should properly expand a mixed template") {
    forAll(ExpandTextWithRepetitionSuccess.mixedTemplates)(_ must expandTextWithRepetition)
  }

  property("TemplateExpander.expandTextWithRepetition should properly fall back to constants") {
    forAll(ExpandTextWithRepetitionSuccess.missingDataWithConstants)(_ must expandTextWithRepetition)
  }

  property("TemplateExpander.expandTextWithRepetition should properly render a non-string JSON scalar") {
    forAll(ExpandTextWithRepetitionSuccess.dataWithNonStringScalars)(_ must expandTextWithRepetition)
  }

  property("TemplateExpander.expandTextWithRepetition should properly render a large JSON number") {
    forAll(ExpandTextWithRepetitionSuccess.dataWithLargeNumbers)(_ must expandTextWithRepetition)
  }

  property("TemplateExpander.expandTextWithRepetition should properly multiply a JSON array") {
    forAll(ExpandTextWithRepetitionSuccess.templatesWithASingleJsonArray)(_ must expandTextWithRepetition)
  }

  property("TemplateExpander.expandTextWithRepetition should properly multiply several JSON arrays") {
    forAll(ExpandTextWithRepetitionSuccess.templatesWithMultipleJsonArrays)(_ must expandTextWithRepetition)
  }

  property("TemplateExpander.expandTextWithRepetition should fail if provided a JSON object") {
    forAll(ExpandFailure.dataWithJsonObjectValues)(_ must refuseToExpandTextWithRepetition)
  }

  //endregion

  //region TemplateExpander.expandJson

  property("TemplateExpander.expandJson should properly handle missing values using placeholders") {
    forAll(ExpandJsonSuccess.dataMissingValues)(_ must expandJson)
  }

  property("TemplateExpander.expandJson should properly handle missing values when forbidden") {
    forAll(ExpandFailure.dataMissingValues)(_ must refuseToExpandJson)
  }

  property("TemplateExpander.expandJson should properly expand a constant template") {
    forAll(ExpandJsonSuccess.constantTemplates)(_ must expandJson)
  }

  property("TemplateExpander.expandJson should properly expand a mixed template") {
    forAll(ExpandJsonSuccess.mixedTemplates)(_ must expandJson)
  }

  property("TemplateExpander.expandJson should properly fall back to constants") {
    forAll(ExpandJsonSuccess.missingDataWithConstants)(_ must expandJson)
  }

  property("TemplateExpander.expandJson should properly inline JSON scalar values") {
    forAll(ExpandJsonSuccess.dataWithScalars)(_ must expandJson)
  }

  property("TemplateExpander.expandJson should properly inline a large JSON number") {
    forAll(ExpandJsonSuccess.dataWithLargeNumbers)(_ must expandJson)
  }

  property("TemplateExpander.expandJson should properly inline a JSON array") {
    forAll(ExpandJsonSuccess.dataWithJsonArray)(_ must expandJson)
  }

  property("TemplateExpander.expandJson should properly inline a JSON object") {
    forAll(ExpandJsonSuccess.dataWithJsonObject)(_ must expandJson)
  }

  property("TemplateExpander.expandJson should fail if the result is not valid JSON") {
    forAll(ExpandFailure.templateAndDataProduceInvalidJSON)(_ must refuseToExpandJson)
  }

  //endregion
}

object TemplateExpanderTest extends DefaultTestInstances {
  private val id: Data.Identifier.Type = Data.Identifier.of("id")
  private val noData: Data = Data.empty(id)
  private val defaultConfig: TemplateConfig = TemplateConfig(
    allowEmpty = false,
    doNotUseExponents = false,
    placeholders = Placeholders(Json.Null, "<missing>"),
    substitutionMarkers = SubstitutionMarkers("{{", "}}")
  )

  case class ExpandSuccess[A](input: Template, constants: Constants, config: TemplateConfig, data: Data, expected: A) {
    def map[B](f: A => B): ExpandSuccess[B] = copy(expected = f(expected))
  }

  case class ExpandFailure(input: Template,
                           constants: Constants = Constants.empty,
                           config: TemplateConfig = defaultConfig,
                           data: Data,
                           expected: NonEmptyList[ExpansionError])
  object ExpandFailure extends EitherValues {
    val dataMissingValues: Gen[ExpandFailure] =
      Arbitrary.arbitrary[Template.Name].map { name =>
        ExpandFailure(
          input = Template.one(Substitution(name)),
          data = Data.empty(id),
          config = defaultConfig.copy(allowEmpty = false),
          expected = NonEmptyList.one(ExpansionError.MissingSubstitution(name))
        )
      }

    val dataWithNonScalarJsonValues: Gen[ExpandFailure] =
      for {
        name <- Arbitrary.arbitrary[Template.Name]
        (value, error) <- Gen.oneOf(
          jsonGens.arrayGen(0 to 10, jsonGens.scalarGen).map(_ -> JsonArrayForbidden(name)),
          jsonGens.objGen(0 to 10, jsonGens.scalarGen).map(_ -> JsonObjectForbidden(name))
        )
      } yield ExpandFailure(
        input = Template.one(Substitution(name)),
        data = Data.of(id)(name -> value),
        expected = NonEmptyList.one(error)
      )

    val dataWithJsonObjectValues: Gen[ExpandFailure] =
      for {
        name <- Arbitrary.arbitrary[Template.Name]
        value <- jsonGens.objGen(0 to 10, jsonGens.scalarGen)
      } yield ExpandFailure(
        input = Template.one(Substitution(name)),
        data = Data.of(id)(name -> value),
        expected = NonEmptyList.one(ExpansionError.JsonObjectForbidden(name))
      )

    def templateAndDataProduceInvalidJSON: Gen[ExpandFailure] = {
      val badStringNesting =
        for {
          name <- Arbitrary.arbitrary[Template.Name]
          json <- Gen.alphaNumChar.string(1 to 30).map(_.asJson)
        } yield {
          val expanded = s"""{"badNesting":"${json.compact}"}"""
          ExpandFailure(
            input = Template.of(Const("""{"badNesting":""""), Substitution(name), Const(""""}""")),
            data = Data.of(id)(name -> json),
            expected = NonEmptyList.one(ExpansionProducedInvalidJson(
              expanded,
              parser.parse(expanded).left.value.show
            ))
          )
        }

      val templateMissingParts =
        for {
          name <- Arbitrary.arbitrary[Template.Name]
          json <- Gen.alphaNumChar.string(1 to 30).map(_.asJson)
        } yield {
          val expanded = s"""{"missingBits":"${json.compact}"""
          ExpandFailure(
            input = Template.of(Const("""{"missingBits":""""), Substitution(name)),
            data = Data.of(id)(name -> json),
            expected = NonEmptyList.one(ExpansionProducedInvalidJson(
              expanded,
              parser.parse(expanded).left.value.show
            ))
          )
        }

      Gen.oneOf(badStringNesting, templateMissingParts)
    }
  }

  type ExpandTextSuccess = ExpandSuccess[String]

  object ExpandTextSuccess {
    def apply(input: Template,
              constants: Constants = Constants.empty,
              config: TemplateConfig = defaultConfig,
              data: Data,
              expected: String): ExpandTextSuccess =
      ExpandSuccess(input, constants, config, data, expected)

    val constantTemplates: Gen[ExpandTextSuccess] =
      Arbitrary.arbitrary[Const].map { c =>
          ExpandTextSuccess(
            input = Template.of(c),
            data = noData,
            expected = c.value
          )
        }

    val substitutionTemplates: Gen[ExpandTextSuccess] =
      for {
        prefix <- Arbitrary.arbitrary[Const]
        (sub, rendered, value) <- standardGens.substitutions.string(Gen.alphaNumChar.string(1 to 20))
        suffix <- Arbitrary.arbitrary[Const]
      } yield ExpandTextSuccess(
        input = Template.of(prefix, sub, suffix),
        data = Data.of(id)(sub.name -> value),
        expected = s"${prefix.value}$rendered${suffix.value}"
      )

    val mixedTemplates: Gen[ExpandTextSuccess] = {
      val singleSubstitutionGen: Gen[Either[(Const, String), (Substitution, String)]] =
        standardGens.substitutions
          .string(Gen.alphaNumChar.string(1 to 20))
          .map {
            case (substitution, str, _) => (substitution, str).asRight[(Const, String)]
          }

      val singleConstGen: Gen[Either[(Const, String), (Substitution, String)]] =
        Arbitrary.arbitrary[Const].map { c =>
          (c, c.value).asLeft[(Substitution, String)]
        }

      val elementsGen: Gen[List[Either[(Const, String), (Substitution, String)]]] =
        Gen.oneOf(singleConstGen, singleSubstitutionGen)
          .list(2 to 10)
          // distinctBy is needed to avoid substitutions with the same name
          .map(_.distinctBy(_.bimap(_._1, _._1.name).fold(_.value, Name.raw)))

      elementsGen.map { elements =>
        ExpandTextSuccess(
          input = Template.fromList(elements.map(_.fold(_._1, _._1))),
          data = Data(id, elements.flatMap(_.toList).map {
            case (substitution, text) => substitution.name -> Json.fromString(text)
          }.toMap),
          expected = elements.map(_.fold(_._2, _._2)).mkString
        )
      }
    }

    val missingDataWithConstants: Gen[ExpandTextSuccess] =
      mixedTemplates.flatMap { test =>
        test.data.values.keys.toList.indices.gen
          .map(_.max(1))
          .map { numberToMove =>
            val (data, constants) = test.data.values.toList.sortBy(_._1).splitAt(numberToMove)
            test.copy(
              data = test.data.copy(values = data.toMap),
              constants = Constants(constants.toMap)
            )
          }
      }

    val dataWithNonStringScalars: Gen[ExpandTextSuccess] = {
      val nulls = standardGens.substitutions.renderedJson(Gen.const("null" -> Json.Null))

      val booleans =
        standardGens.substitutions.renderedJson(Arbitrary.arbitrary[Boolean].map {
          if (_) "true" -> Json.True else "false" -> Json.False
        })

      val numbers = standardGens.substitutions.renderedJson {
        Gen.chooseNum(-1000L, 1000L).map(long => s"$long" -> long.asJson)
      }

      Gen.oneOf(nulls, booleans, numbers).map {
        case (substitution, str, json) => ExpandTextSuccess(
          input = Template.of(substitution),
          data = Data.of(id)(substitution.name -> json),
          expected = str
        )
      }
    }

    val dataWithLargeNumbers: Gen[ExpandTextSuccess] = {
      val renderPlain =
        standardGens.substitutions
          .renderedJson(genBigNumbers.map(num => num.bigDecimal.toPlainString -> num.asJson))
          .map(_ -> defaultConfig.copy(doNotUseExponents = true))

      val renderUsingExponents =
        standardGens.substitutions
          .renderedJson(genBigNumbers.map(num => num.bigDecimal.toEngineeringString -> num.asJson))
          .map(_ -> defaultConfig.copy(doNotUseExponents = false))

      Gen.oneOf(renderPlain, renderUsingExponents).map {
        case ((substitution, str, json), config) => ExpandTextSuccess(
          input = Template.of(substitution),
          data = Data.of(id)(substitution.name -> json),
          config = config,
          expected = str
        )
      }
    }

    val dataMissingValues: Gen[ExpandTextSuccess] =
      for {
        prefix <- Arbitrary.arbitrary[Const]
        name <- Arbitrary.arbitrary[Template.Name]
        suffix <- Arbitrary.arbitrary[Const]
      } yield ExpandTextSuccess(
        input = Template.of(prefix, Substitution(name), suffix),
        data = Data.empty(id),
        config = defaultConfig.copy(allowEmpty = true),
        expected = s"${prefix.value}${defaultConfig.placeholders.text}${suffix.value}"
      )
  }

  type ExpandTextWithRepetitionSuccess = ExpandSuccess[List[String]]

  object ExpandTextWithRepetitionSuccess {
    def apply(input: Template,
              constants: Constants = Constants.empty,
              config: TemplateConfig = defaultConfig,
              data: Data,
              expected: List[String]): ExpandTextWithRepetitionSuccess =
      ExpandSuccess(input, constants, config, data, expected.sorted)


    val constantTemplates: Gen[ExpandTextWithRepetitionSuccess] =
      ExpandTextSuccess.constantTemplates.map(_.map(_ :: Nil))

    val substitutionTemplates: Gen[ExpandTextWithRepetitionSuccess] =
      ExpandTextSuccess.substitutionTemplates.map(_.map(_ :: Nil))

    val mixedTemplates: Gen[ExpandTextWithRepetitionSuccess] =
      ExpandTextSuccess.mixedTemplates.map(_.map(_ :: Nil))

    val missingDataWithConstants: Gen[ExpandTextWithRepetitionSuccess] =
      ExpandTextSuccess.missingDataWithConstants.map(_.map(_ :: Nil))

    val dataWithNonStringScalars: Gen[ExpandTextWithRepetitionSuccess] =
      ExpandTextSuccess.dataWithNonStringScalars.map(_.map(_ :: Nil))

    val dataWithLargeNumbers: Gen[ExpandTextWithRepetitionSuccess] =
      ExpandTextSuccess.dataWithLargeNumbers.map(_.map(_ :: Nil))

    val missingValues: Gen[ExpandTextWithRepetitionSuccess] =
      ExpandTextSuccess.dataMissingValues.map(_.map(_ :: Nil))

    val templatesWithASingleJsonArray: Gen[ExpandTextWithRepetitionSuccess] =
      for {
        prefix <- Arbitrary.arbitrary[Const]
        (substitution, renders, value) <- standardGens.substitutions.stringList {
          Gen.alphaNumChar.string(0 to 5).list(0 to 5)
        }
        suffix <- Arbitrary.arbitrary[Const]
      } yield ExpandTextWithRepetitionSuccess(
        input = Template.of(prefix, substitution, suffix),
        data = Data.of(id)(substitution.name -> value),
        expected = renders.map(r => s"${prefix.value}$r${suffix.value}")
      )

    val templatesWithMultipleJsonArrays: Gen[ExpandTextWithRepetitionSuccess] =
      for {
        prefix <- Arbitrary.arbitrary[Const]
        (substitution1, renders1, value1) <- standardGens.substitutions.stringList {
          Gen.alphaNumChar.string(0 to 5).list(0 to 5)
        }
        sep <- Arbitrary.arbitrary[Const]
        (substitution2, renders2, value2) <- standardGens.substitutions.stringList {
          Gen.alphaNumChar.string(0 to 5).list(0 to 5)
        }
        suffix <- Arbitrary.arbitrary[Const]
      } yield {
        val name1 = Template.Name(s"First:${Template.Name.raw(substitution1.name)}")
        val name2 = Template.Name(s"Second:${Template.Name.raw(substitution2.name)}")
        ExpandTextWithRepetitionSuccess(
          input = Template.of(prefix, Substitution(name1), sep, Substitution(name2), suffix),
          data = Data.of(id)(name1 -> value1, name2 -> value2),
          expected = for {
            first <- renders1
            second <- renders2
          } yield s"${prefix.value}$first${sep.value}$second${suffix.value}"
        )
      }
  }

  type ExpandJsonSuccess = ExpandSuccess[Json]

  object ExpandJsonSuccess {
    def apply(input: Template,
              constants: Constants = Constants.empty,
              config: TemplateConfig = defaultConfig,
              data: Data,
              expected: Json): ExpandJsonSuccess =
      ExpandSuccess(input, constants, config, data, expected)

    val standardJsonGen: Gen[Json] = Gen.oneOf(
      jsonGens.scalarGen,
      jsonGens.arrayGen(0 to 10, jsonGens.scalarGen),
      jsonGens.objGen(0 to 10, jsonGens.scalarGen),
      jsonGens.graph(0 to 10, 0 to 10, 1 to 2)
    )
    implicit val arbJson: Arbitrary[Json] = Arbitrary(standardJsonGen)

    val constantTemplates: Gen[ExpandJsonSuccess] =
      Arbitrary.arbitrary[Json].map { json =>
        ExpandJsonSuccess(
          input = Template.of(Const(json.compact)),
          data = Data.empty(id),
          expected = json
        )
      }

    val mixedTemplates: Gen[ExpandJsonSuccess] =
      for {
        prefix <- Arbitrary.arbitrary[Const]
        (sub, rendered, value) <- standardGens.substitutions.renderedJson {
          jsonGens.numberGen.map {
            case (l, j) => s"$l" -> j
          }
        }
        suffix <- Arbitrary.arbitrary[Const]
      } yield ExpandJsonSuccess(
        input = Template.of(Const("\""), prefix, sub, suffix, Const("\"")),
        data = Data.of(id)(sub.name -> value),
        expected = s"${prefix.value}$rendered${suffix.value}".asJson
      )

    val missingDataWithConstants: Gen[ExpandJsonSuccess] =
      mixedTemplates.flatMap { test =>
        test.data.values.keys.toList.indices.gen
          .map(_.max(1))
          .map { numberToMove =>
            val (data, constants) = test.data.values.toList.sortBy(_._1).splitAt(numberToMove)
            test.copy(
              data = test.data.copy(values = data.toMap),
              constants = Constants(constants.toMap)
            )
          }

      }

    private def jsonSubstitutions(keyGen: Gen[Json]): Gen[ExpandJsonSuccess] =
      for {
        name <- Arbitrary.arbitrary[Template.Name]
        json <- keyGen
        wrapper <- jsonGens.fieldNames
      } yield
        ExpandJsonSuccess(
          input = Template.of(Const(s"""{"$wrapper":"""), Substitution(name), Const("}")),
          data = Data.of(id)(name -> json),
          expected = Json.obj(wrapper -> json)
        )

    val dataWithScalars: Gen[ExpandJsonSuccess] = jsonSubstitutions(jsonGens.scalarGen)

    val dataWithLargeNumbers: Gen[ExpandJsonSuccess] = jsonSubstitutions(genBigNumbers.map(_.asJson))

    val dataMissingValues: Gen[ExpandJsonSuccess] =
      for {
        prefix <- Arbitrary.arbitrary[Const]
        name <- Arbitrary.arbitrary[Template.Name]
        suffix <- Arbitrary.arbitrary[Const]
      } yield ExpandJsonSuccess(
        input = Template.of(Const("\""),prefix, Substitution(name), suffix, Const("\"")),
        data = Data.empty(id),
        config = defaultConfig.copy(allowEmpty = true),
        expected =
          s"${prefix.value}${defaultConfig.placeholders.json.compact}${suffix.value}".asJson
      )

    val dataWithJsonArray: Gen[ExpandJsonSuccess] =
      jsonSubstitutions(jsonGens.arrayGen(0 to 10, jsonGens.scalarGen))

    val dataWithJsonObject: Gen[ExpandJsonSuccess] =
      jsonSubstitutions(jsonGens.objGen(0 to 10, jsonGens.scalarGen))
  }
}