package peschke.bulk_calls.services

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.parse.Parser.Expectation
import cats.syntax.either._
import cats.syntax.semigroup._
import cats.syntax.applicative._
import cats.{Id, Semigroup}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.cats.instances._
import org.scalatest.matchers.Matcher
import peschke.bulk_calls.{DefaultTestInstances, PropertyTest}
import peschke.bulk_calls.PropertyTest.syntax._
import peschke.bulk_calls.config.TemplateConfig.SubstitutionMarkers
import peschke.bulk_calls.models.Template
import peschke.bulk_calls.models.Template.Element
import peschke.bulk_calls.models.Template.Element._
import peschke.bulk_calls.services.TemplateParser.InvalidTemplate
import peschke.bulk_calls.services.TemplateParserTest.{FailureTest, SuccessTest}

class TemplateParserTest extends PropertyTest {
  def successfullyParse: Matcher[SuccessTest] =
    Matcher { test =>
      val parser = new TemplateParser.Default[Id](test.markers)
      be(test.template.asRight[InvalidTemplate]).apply(parser.parse(test.input))
    }

  def failToParse: Matcher[FailureTest] =
    Matcher { test =>
      val parser = new TemplateParser.Default[Id](test.markers)
      be(NonEmptyList.one(test.failure).asLeft[Template]).apply(parser.parse(test.input))
    }

  property("TemplateParser should parse a string without markers") {
    forAll(SuccessTest.constants)(_ must successfullyParse)
  }

  property("TemplateParser should parse a string that is just a substitution") {
    forAll(SuccessTest.substitutions)(_ must successfullyParse)
  }

  property("TemplateParser should parse a string that contains a single substitution") {
    forAll(SuccessTest.containsOneSubstitution)(_ must successfullyParse)
  }

  property("TemplateParser should parse a string that contains multiple substitutions") {
    forAll(SuccessTest.containsMultipleSubstitutions)(_ must successfullyParse)
  }

  property("TemplateParser should ignore an unmatched close marker") {
    forAll(SuccessTest.unmatchedCloseMarkers)(_ must successfullyParse)
  }

  property("TemplateParser should fail to parse a string missing a close marker") {
    forAll(FailureTest.missingCloseMarker)(_ must failToParse)
  }

  property("TemplateParser should fail to parse a string missing a name after an open marker") {
    forAll(FailureTest.openMarkersWithoutNames)(_ must failToParse)
  }
}
object TemplateParserTest extends DefaultTestInstances {

  val substitutionMarkers: Gen[SubstitutionMarkers] =
    Gen.oneOf(
      SubstitutionMarkers("{{", "}}"),
      SubstitutionMarkers("{", "}"),
      SubstitutionMarkers("[[", "]]"),
      SubstitutionMarkers("[", "]"),
      SubstitutionMarkers("((", "))"),
      SubstitutionMarkers("(", ")")
    )

  final case class SuccessTest(input: String, markers: SubstitutionMarkers, template: Template)
  object SuccessTest {
    def fromElement(i: String, markers: SubstitutionMarkers, e: Element): SuccessTest =
      fromElements(i, markers, e.pure[NonEmptyList])

    def fromElements(i: String, markers: SubstitutionMarkers, e: NonEmptyList[Element]): SuccessTest =
      SuccessTest(i, markers, Template.fromNel(e))

    def fromElements(i: String, markers: SubstitutionMarkers, e0: Element, eN: Element*): SuccessTest =
      SuccessTest(i, markers, Template.fromList(e0 :: eN.toList))

    implicit final val semigroup: Semigroup[SuccessTest] = Semigroup.instance[SuccessTest](
      (a, b) => SuccessTest(
        a.input.combine(b.input),
        a.markers,
        a.template.combine(b.template)
      )
    )

    val constants: Gen[SuccessTest] = substitutionMarkers.flatMap(constants(_))
    def constants(markers: SubstitutionMarkers): Gen[SuccessTest] =
      Arbitrary.arbitrary[Const].map(c => SuccessTest.fromElement(c.value, markers, c))

    val substitutions: Gen[SuccessTest] = substitutionMarkers.flatMap(substitutions(_))
    def substitutions(markers: SubstitutionMarkers): Gen[SuccessTest] =
      Arbitrary.arbitrary[Template.Name].map { name =>
          SuccessTest.fromElement(
            s"${markers.open}${Template.Name.raw(name)}${markers.close}",
            markers,
            Substitution(name)
          )
        }

    val containsOneSubstitution: Gen[SuccessTest] = substitutionMarkers.flatMap(containsOneSubstitution(_))
    def containsOneSubstitution(markers: SubstitutionMarkers): Gen[SuccessTest] =
      NonEmptyList.of(constants(markers), substitutions(markers), constants(markers)).reduce

    val containsMultipleSubstitutions: Gen[SuccessTest] = substitutionMarkers.flatMap(containsMultipleSubstitutions(_))
    def containsMultipleSubstitutions(markers: SubstitutionMarkers): Gen[SuccessTest] =
      containsOneSubstitution(markers).nel(1 to 10).map(_.reduce)

    val unmatchedCloseMarkers: Gen[SuccessTest] = substitutionMarkers.flatMap(unmatchedCloseMarkers(_))
    def unmatchedCloseMarkers(markers: SubstitutionMarkers): Gen[SuccessTest] =
      for {
        prefix <- Gen.alphaNumChar.string(0 to 10)
        input = s"$prefix${markers.close}"
      } yield SuccessTest.fromElement(input, markers, Const(input))
  }

  final case class FailureTest(input: String, markers: SubstitutionMarkers, failure: InvalidTemplate)
  object FailureTest {

    def apply(input: String,
              markers: SubstitutionMarkers,
              expectation0: Parser.Expectation,
              expectationN: Parser.Expectation*,
             ): FailureTest =
      FailureTest(
        input,
        markers,
        InvalidTemplate(
          markers,
          Parser.Error(
            input,
            expectation0.offset,
            NonEmptyList(expectation0, expectationN.toList))
        )
      )

    private def missingCloseBraceExpectation(failedAt: Int, markers: SubstitutionMarkers): Expectation =
      Expectation.WithContext(
        TemplateParser.Context.Close,
        markers.close.headOption
          .flatMap { closeChar =>
            Option.when(markers.close.length == 1)(Expectation.InRange(failedAt, closeChar, closeChar))
          }
          .getOrElse(Expectation.OneOfStr(failedAt, markers.close :: Nil))
      )

    private def missingNameExpectation(failedAt: Int): Expectation =
      Expectation.WithContext(
        TemplateParser.Context.Name,
        Expectation.InRange(failedAt, Char.MinValue, Char.MaxValue)
      )

    val missingCloseMarker: Gen[FailureTest] = substitutionMarkers.flatMap(missingCloseMarker(_))
    def missingCloseMarker(markers: SubstitutionMarkers): Gen[FailureTest] =
      for {
        prefix <-Gen.alphaNumChar.string(0 to 10)
        name <- Gen.alphaNumChar.string(1 to 10)
      } yield FailureTest(
        s"$prefix${markers.open}$name",
        markers,
        missingCloseBraceExpectation(prefix.length + markers.open.length + name.length, markers)
      )

    val openMarkersWithoutNames: Gen[FailureTest] = substitutionMarkers.flatMap(openMarkersWithoutNames(_))
    def openMarkersWithoutNames(markers: SubstitutionMarkers): Gen[FailureTest] =
      Gen.alphaNumChar.string(0 to 10).map { prefix =>
        FailureTest(
          s"$prefix${markers.open}",
          markers,
          missingNameExpectation(prefix.length + markers.open.length)
        )
      }
  }
}