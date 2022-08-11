package peschke.bulk_calls.models

import cats.syntax.either._
import io.circe.CursorOp.{DownField, DownN}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.{MatchResult, Matcher}
import peschke.bulk_calls.PropertyTest.syntax._
import peschke.bulk_calls.models.JsonPathTest.{arrayIndexes, complexPaths, indexFieldsWithUnescapedQuote, indexFieldsWithUnmatchedBraces, indexFieldsWithUnmatchedQuote, indexStyleFields, invalidMethodFields, methodStyleFields, negativeArrayIndexes}
import peschke.bulk_calls.{DefaultTestInstances, WordTest}

import scala.util.matching.Regex

class JsonPathTest extends WordTest {
  val parseSuccessfully: Matcher[JsonPath] = Matcher { path =>
    be(path.asRight).apply(JsonPath.from(path.raw))
  }

  val failToParse: Matcher[String] = Matcher { input =>
    val result = JsonPath.from(input)
    MatchResult(
      result.isLeft,
      "JsonPath.from({0}) did not fail, but returned {1}",
      "JsonPath.from({0}) failed with error {1}",
      Vector(input, result),
      Vector(input, result)
    )
  }

  "JsonPath.from" should {
    "parse method-style fields" in forAll(methodStyleFields)(_ must parseSuccessfully)

    "parse index-style fields" in forAll(indexStyleFields)(_ must parseSuccessfully)

    "parse array indexes" in forAll(arrayIndexes)(_ must parseSuccessfully)

    "parse a single '.'" in {
      JsonPath.from(".").value mustBe JsonPath(Nil, ".")
    }

    "parse complex paths" in forAll(complexPaths)(_ must parseSuccessfully)

    "reject method-style fields with invalid characters" in forAll(invalidMethodFields)(_ must failToParse)

    "reject index-style fields with unescaped quotes" in forAll(indexFieldsWithUnescapedQuote)(_ must failToParse)

    "reject negative array indexes" in forAll(negativeArrayIndexes)(_ must failToParse)

    "reject unmatched quotes" in forAll(indexFieldsWithUnmatchedQuote)(_ must failToParse)

    "reject unmatched braces" in forAll(indexFieldsWithUnmatchedBraces)(_ must failToParse)
  }
}
object JsonPathTest extends DefaultTestInstances {
  private val validLeadingCharForMethodStyleFields = Gen.oneOf(Gen.alphaChar, Gen.const('_'))
  private val validTrailingCharForMethodStyleFields = Gen.oneOf(Gen.alphaNumChar, Gen.const('_'))

  private val unescapedQuoteRegex = raw"""(\\*)"""".r
  private def escapedIndexStyleFieldName(length: Range): Gen[String] =
    Arbitrary.arbitrary[Char].string(length)
      .map(unescapedQuoteRegex.replaceAllIn(_, _ match {
        case Regex.Groups(escapes) =>
          if (escapes.length % 2 == 0) raw"""$$1\"""" else raw"""$$1""""
      }))

  val methodStyleFields: Gen[JsonPath] =
    for {
      c0 <- validLeadingCharForMethodStyleFields
      cN <- validTrailingCharForMethodStyleFields.string(0 to 20)
    } yield {
      val field = cN.prepended(c0)
      JsonPath(DownField(field) :: Nil, field.prepended('.'))
    }

  val indexStyleFields: Gen[JsonPath] =
    escapedIndexStyleFieldName(1 to 20).map { fieldName =>
      JsonPath(DownField(fieldName) :: Nil, s"""["$fieldName"]""")
    }

  val arrayIndexes: Gen[JsonPath] =
    (0 to Int.MaxValue).gen.map { index =>
      JsonPath(DownN(index) :: Nil, s"[$index]")
    }

  val complexPaths: Gen[JsonPath] = {
    val subPaths = Gen.oneOf(methodStyleFields, indexStyleFields, arrayIndexes)
    subPaths.nel(1 to 10).map(_.reduce)
  }

  val invalidMethodFields: Gen[String] = {
    val badChars = Gen.oneOf("$&[{}(=*)+]!")
    val invalidLeadingChar =
      for {
        first <- Gen.oneOf(Gen.numChar, badChars)
        rest <- validTrailingCharForMethodStyleFields.string(0 to 10)
      } yield s".$first$rest"

    val invalidTrailingChar =
      for {
        first <- validLeadingCharForMethodStyleFields
        prefix <- validTrailingCharForMethodStyleFields.string(0 to 5)
        badChar <- badChars
        suffix <- validTrailingCharForMethodStyleFields.string(0 to 5)
      } yield s".$first$prefix$badChar$suffix"

    val missingPeriod = methodStyleFields.map(_.raw.drop(1))

    Gen.oneOf(missingPeriod, invalidLeadingChar, invalidTrailingChar)
  }

  val indexFieldsWithUnescapedQuote: Gen[String] =
    for {
      prefix <- escapedIndexStyleFieldName(0 to 5)
      suffix <- escapedIndexStyleFieldName(0 to 5)
    } yield s"""$prefix"$suffix"""

  val negativeArrayIndexes: Gen[String] = (Int.MinValue to -1).gen.map(index => s"[$index]")

  val indexFieldsWithUnmatchedQuote: Gen[String] =
    escapedIndexStyleFieldName(1 to 20).map(name => s"""["$name]""")

  val indexFieldsWithUnmatchedBraces: Gen[String] =
    Gen.oneOf(
      escapedIndexStyleFieldName(1 to 20).map(name => s"""["$name""""),
      (0 to Int.MaxValue).gen.map(index => s"[$index")
    )
}
