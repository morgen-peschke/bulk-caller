package peschke.bulk_calls.services

import cats.Id
import cats.data.NonEmptyList
import io.circe.{CursorOp, Json}
import io.circe.syntax._
import org.scalacheck.Gen
import org.scalatest.matchers.Matcher
import peschke.bulk_calls.PropertyTest
import peschke.bulk_calls.PropertyTest.{jsonGens, rangeGen}
import peschke.bulk_calls.models.{JsonPath, When}
import peschke.bulk_calls.services.WhenCheckerTest.NestedGens.{AnyGens, AtGens, ExistsGens, ForallGens, NotGens}
import peschke.bulk_calls.services.WhenCheckerTest.UnnestedGens.ContainsGen

import scala.jdk.CollectionConverters._

class WhenCheckerTest extends PropertyTest {

  import peschke.bulk_calls.services.WhenCheckerTest.UnnestedGens._
  import peschke.bulk_calls.services.WhenCheckerTest._

  val checker: WhenChecker[Id] = WhenChecker.default[Id]

  def acceptJson: Matcher[JsonTest] = be(true).compose(_.run(checker))

  def rejectJson: Matcher[JsonTest] = be(false).compose(_.run(checker))

  def acceptString: Matcher[StringTest] = be(true).compose(_.run(checker))

  def rejectString: Matcher[StringTest] = be(false).compose(_.run(checker))

  //region Unnested Gen invariants check

  property("NonRecursiveGen.acceptingJsonTests should produce passing tests") {
    forAll(UnnestedGens.acceptingJsonTests)(_ must acceptJson)
  }

  property("NonRecursiveGen.rejectingJsonTests should produce passing tests") {
    forAll(UnnestedGens.rejectingJsonTests)(_ must rejectJson)
  }

  property("NonRecursiveGen.acceptingStringTests should produce passing tests") {
    forAll(UnnestedGens.acceptingStringTests)(_ must acceptString)
  }

  property("NonRecursiveGen.rejectingStringTests should produce passing tests") {
    forAll(UnnestedGens.rejectingStringTests)(_ must rejectString)
  }

  //endregion

  //region Not Properties

  property("Not should accept json when wrapped When rejects them") {
    forAll(NotGens.innerWhenRejectsJson)(_ must acceptJson)
  }

  property("Not should reject json when wrapped When accepts them") {
    forAll(NotGens.innerWhenAcceptsJson)(_ must rejectJson)
  }

  property("Not should accept strings when wrapped When rejects them") {
    forAll(NotGens.innerWhenRejectsStrings)(_ must acceptString)
  }

  property("Not should reject strings when wrapped When accepts them") {
    forAll(NotGens.innerWhenAcceptsStrings)(_ must rejectString)
  }

  //endregion

  //region Equality Properties

  property("Equality should accept equal json values") {
    forAll(EqualityGens.equalJson)(_ must acceptJson)
  }

  property("Equality should accept string-encoded equal values") {
    forAll(EqualityGens.equalStrings)(_ must acceptString)
  }

  property("Equality should reject unequal json values") {
    forAll(EqualityGens.unequalJson)(_ must rejectJson)
  }

  //endregion

  //region Exists Properties

  property("Exists should reject json if no nested Whens accept") {
    forAll(ExistsGens.jsonNoNestedWhensPasses)(_ must rejectJson)
  }

  property("Exists should accept json if at least one nested When accepts") {
    forAll(ExistsGens.jsonAtLeastOneNestedWhenPasses)(_ must acceptJson)
  }

  property("Exists should accept json if all nested Whens accept") {
    forAll(ExistsGens.jsonAllNestedWhensPass)(_ must acceptJson)
  }

  property("Exists should reject strings if no nested Whens accept") {
    forAll(ExistsGens.stringNoNestedWhensPasses)(_ must rejectString)
  }

  property("Exists should accept strings if at least one nested When accepts") {
    forAll(ExistsGens.stringAtLeastOneNestedWhenPasses)(_ must acceptString)
  }

  property("Exists should accept strings if all nested Whens accept") {
    forAll(ExistsGens.stringAllNestedWhensPass)(_ must acceptString)
  }

  //endregion

  //region Forall Properties

  property("Forall should reject json if no nested Whens accept") {
    forAll(ForallGens.jsonNoNestedWhensPasses)(_ must rejectJson)
  }

  property("Forall should reject json if fewer than all nested Whens accept") {
    forAll(ForallGens.jsonAtLeastOneNestedWhenPasses)(_ must rejectJson)
  }

  property("Forall should accept json if all nested Whens accept") {
    forAll(ForallGens.jsonAllNestedWhensPass)(_ must acceptJson)
  }

  property("Forall should reject strings if no nested Whens accept") {
    forAll(ForallGens.stringNoNestedWhensPasses)(_ must rejectString)
  }

  property("Forall should reject strings if fewer than all nested Whens accept") {
    forAll(ForallGens.stringAtLeastOneNestedWhenPasses)(_ must rejectString)
  }

  property("Forall should accept strings if all nested Whens accept") {
    forAll(ForallGens.stringAllNestedWhensPass)(_ must acceptString)
  }

  //endregion

  // region LessThan Properties

  property("LessThan should accept smaller json values") {
    forAll(LessThanGens.smallerJson)(_ must acceptJson)
  }

  property("LessThan should reject larger json values") {
    forAll(LessThanGens.largerJson)(_ must rejectJson)
  }

  property("LessThan should reject equal json values") {
    forAll(LessThanGens.equalJson)(_ must rejectJson)
  }

  property("LessThan should accept smaller string values") {
    forAll(LessThanGens.smallerStrings)(_ must acceptString)
  }

  property("LessThan should reject larger string values") {
    forAll(LessThanGens.largerStrings)(_ must rejectString)
  }

  property("LessThan should reject equal string values") {
    forAll(LessThanGens.equalStrings)(_ must rejectString)
  }

  //endregion

  // region LessThanOrEqual Properties

  property("LessThanOrEqual should accept smaller json values") {
    forAll(LessThanOrEqualGens.smallerJson)(_ must acceptJson)
  }

  property("LessThanOrEqual should reject larger json values") {
    forAll(LessThanOrEqualGens.largerJson)(_ must rejectJson)
  }

  property("LessThanOrEqual should accept equal json values") {
    forAll(LessThanOrEqualGens.equalJson)(_ must acceptJson)
  }

  property("LessThanOrEqual should accept smaller string values") {
    forAll(LessThanOrEqualGens.smallerStrings)(_ must acceptString)
  }

  property("LessThanOrEqual should reject larger string values") {
    forAll(LessThanOrEqualGens.largerStrings)(_ must rejectString)
  }

  property("LessThanOrEqual should accept equal string values") {
    forAll(LessThanOrEqualGens.equalStrings)(_ must acceptString)
  }

  //endregion

  // region GreaterThan Properties

  property("GreaterThan should reject smaller json values") {
    forAll(GreaterThanGens.smallerJson)(_ must rejectJson)
  }

  property("GreaterThan should accept larger json values") {
    forAll(GreaterThanGens.largerJson)(_ must acceptJson)
  }

  property("GreaterThan should reject equal json values") {
    forAll(GreaterThanGens.equalJson)(_ must rejectJson)
  }

  property("GreaterThan should reject smaller string values") {
    forAll(GreaterThanGens.smallerStrings)(_ must rejectString)
  }

  property("GreaterThan should accept larger string values") {
    forAll(GreaterThanGens.largerStrings)(_ must acceptString)
  }

  property("GreaterThan should reject equal string values") {
    forAll(GreaterThanGens.equalStrings)(_ must rejectString)
  }

  //endregion

  // region GreaterThanOrEqual Properties

  property("GreaterThanOrEqual should reject smaller json values") {
    forAll(GreaterThanOrEqualGens.smallerJson)(_ must rejectJson)
  }

  property("GreaterThanOrEqual should accept larger json values") {
    forAll(GreaterThanOrEqualGens.largerJson)(_ must acceptJson)
  }

  property("GreaterThanOrEqual should accept equal json values") {
    forAll(GreaterThanOrEqualGens.equalJson)(_ must acceptJson)
  }

  property("GreaterThanOrEqual should reject smaller string values") {
    forAll(GreaterThanOrEqualGens.smallerStrings)(_ must rejectString)
  }

  property("GreaterThanOrEqual should accept larger string values") {
    forAll(GreaterThanOrEqualGens.largerStrings)(_ must acceptString)
  }

  property("GreaterThanOrEqual should accept equal string values") {
    forAll(GreaterThanOrEqualGens.equalStrings)(_ must acceptString)
  }

  //endregion

  //region At Properties

  property("At should accept json when the nested When accepts the value at the path") {
    forAll(AtGens.innerWhenAcceptsJson)(_ must acceptJson)
  }

  property("At should reject json when the nested When rejects the value at the path") {
    forAll(AtGens.innerWhenRejectsJson)(_ must rejectJson)
  }

  property("At should reject json when the target does not contain the desired path") {
    forAll(AtGens.pathDoesNotExistInJson)(_ must rejectJson)
  }

  //endregion

  //region Any Properties

  property("Any should reject scalar json") {
    forAll(AnyGens.scalarJson)(_ must rejectJson)
  }

  property("Any should reject an array when empty") {
    forAll(AnyGens.arrayIsEmpty)(_ must rejectJson)
  }

  property("Any should reject an object when empty") {
    forAll(AnyGens.objIsEmpty)(_ must rejectJson)
  }

  property("Any should reject an array when no elements match") {
    forAll(AnyGens.noElementsMatch)(_ must rejectJson)
  }

  property("Any should reject an object when no values match") {
    forAll(AnyGens.noValuesMatch)(_ must rejectJson)
  }

  property("Any should accept an array when one element matches") {
    forAll(AnyGens.oneElementMatches)(_ must acceptJson)
  }

  property("Any should accept an object when one values matches") {
    forAll(AnyGens.oneValueMatches)(_ must acceptJson)
  }

  property("Any should accept an array when all elements match") {
    forAll(AnyGens.allElementsMatch)(_ must acceptJson)
  }

  property("Any should accept an object when all elements match") {
    forAll(AnyGens.allValuesMatch)(_ must acceptJson)
  }

  //endregion

  //region StartsWith Properties

  property("StartsWith should accept equal strings") {
    forAll(StartsWithGen.equalRawStrings)(_ must acceptString)
  }

  property("StartsWith should accept strings with a matching prefix") {
    forAll(StartsWithGen.rawStringsWithPrefix)(_ must acceptString)
  }

  property("StartsWith should reject strings without a matching prefix") {
    forAll(StartsWithGen.rawStringsWithoutPrefix)(_ must rejectString)
  }

  property("StartsWith should accept equal json strings") {
    forAll(StartsWithGen.equalJsonStrings)(_ must acceptJson)
  }

  property("StartsWith should accept json strings with a matching prefix") {
    forAll(StartsWithGen.jsonStringsWithPrefix)(_ must acceptJson)
  }

  property("StartsWith should reject json strings without a matching prefix") {
    forAll(StartsWithGen.jsonStringsWithoutPrefix)(_ must rejectJson)
  }

  //endregion

  //region EndsWith Properties

  property("EndsWith should accept equal strings") {
    forAll(EndsWithGen.equalRawStrings)(_ must acceptString)
  }

  property("EndsWith should accept strings with a matching prefix") {
    forAll(EndsWithGen.rawStringsWithSuffix)(_ must acceptString)
  }

  property("EndsWith should reject strings without a matching prefix") {
    forAll(EndsWithGen.rawStringsWithoutSuffix)(_ must rejectString)
  }

  property("EndsWith should accept equal json strings") {
    forAll(EndsWithGen.equalJsonStrings)(_ must acceptJson)
  }

  property("EndsWith should accept json strings with a matching prefix") {
    forAll(EndsWithGen.jsonStringsWithSuffix)(_ must acceptJson)
  }

  property("EndsWith should reject json strings without a matching prefix") {
    forAll(EndsWithGen.jsonStringsWithoutSuffix)(_ must rejectJson)
  }

  //endregion

  //region Contains Properties

  property("Contains should accept equal strings") {
    forAll(ContainsGen.equalRawStrings)(_ must acceptString)
  }

  property("Contains should accept strings with a matching substring") {
    forAll(ContainsGen.rawStringsWithSubstring)(_ must acceptString)
  }

  property("Contains should reject strings without a matching substring") {
    forAll(ContainsGen.rawStringsWithoutSubstring)(_ must rejectString)
  }

  property("Contains should accept equal json strings") {
    forAll(ContainsGen.equalJsonStrings)(_ must acceptJson)
  }

  property("Contains should accept json strings with a matching substring") {
    forAll(ContainsGen.jsonStringsWithSubstring)(_ must acceptJson)
  }

  property("Contains should reject json strings without a matching substring") {
    forAll(ContainsGen.jsonStringsWithoutSubstring)(_ must rejectJson)
  }

  //endregion
}

object WhenCheckerTest {
  final case class JsonTest(when: When, input: Json) {
    def run(checker: WhenChecker[Id]): Boolean = checker.check(when, input)
  }

  final case class StringTest(when: When, input: String) {
    def run(checker: WhenChecker[Id]): Boolean = checker.check(when, input)
  }

  abstract class NumberComparisonGens[W <: When](wrap: BigDecimal => W) {
    val smallerJson: Gen[JsonTest] =
      for {
        (smaller, input) <- jsonGens.numberGen
        delta <- Gen.chooseNum(1, 1000L)
      } yield JsonTest(wrap(BigDecimal(smaller + delta)), input)

    val largerJson: Gen[JsonTest] =
      for {
        (larger, input) <- jsonGens.numberGen
        delta <- Gen.chooseNum(1, 1000L)
      } yield JsonTest(wrap(BigDecimal(larger - delta)), input)

    val equalJson: Gen[JsonTest] = jsonGens.numberGen.map {
      case (bigDec, input) => JsonTest(wrap(BigDecimal(bigDec)), input)
    }

    val smallerStrings: Gen[StringTest] =
      for {
        (smaller, _) <- jsonGens.numberGen
        delta <- Gen.chooseNum(1, 1000L)
      } yield StringTest(wrap(BigDecimal(smaller + delta)), s"$smaller")

    val largerStrings: Gen[StringTest] =
      for {
        (larger, _) <- jsonGens.numberGen
        delta <- Gen.chooseNum(1, 1000L)
      } yield StringTest(wrap(BigDecimal(larger - delta)), s"$larger")

    val equalStrings: Gen[StringTest] = jsonGens.numberGen.map {
      case (long, _) => StringTest(wrap(BigDecimal(long)), s"$long")
    }
  }

  trait StringComparisonGens {
    protected def adaptStringToJsonTest(tests: Gen[StringTest]): Gen[JsonTest] =
      tests.map(st => JsonTest(st.when, Json.fromString(st.input)))
  }

  sealed trait UnnestedGens extends enumeratum.EnumEntry {
    def acceptingJsonTests: Gen[JsonTest]

    def rejectingJsonTests: Gen[JsonTest]

    def acceptingStringTests: Gen[StringTest]

    def rejectingStringTests: Gen[StringTest]
  }

  object UnnestedGens extends enumeratum.Enum[UnnestedGens] {

    object EqualityGens extends UnnestedGens {
      val equalJson: Gen[JsonTest] =
        jsonGens
          .graph(
            objFieldCount = 0 to 10,
            arrayLength = 0 to 10,
            maxDepth = 0 to 5
          )
          .map(input => JsonTest(When.Equal(input), input))

      val equalStrings: Gen[StringTest] =
        jsonGens.stringGen.map {
          case (raw, json) =>
            StringTest(When.Equal(json), raw)
        }

      val unequalString: Gen[StringTest] =
        Gen.oneOf(
          jsonGens.numberGen.map {
            case (long, json) =>
              StringTest(When.Equal(json), s"${long + 5}")
          },
          jsonGens.numberGen.map {
            case (long, json) =>
              StringTest(When.Equal(json), BigDecimal(long + 5).bigDecimal.toEngineeringString)
          }
        )

      val unequalJson: Gen[JsonTest] = {
        jsonGens
          .different {
            jsonGens.graph(
              objFieldCount = 0 to 10,
              arrayLength = 0 to 10,
              maxDepth = 0 to 5
            )
          }
          .map {
            case (expected, input) => JsonTest(When.Equal(expected), input)
          }
      }

      val acceptingJsonTests: Gen[JsonTest] = equalJson
      val rejectingJsonTests: Gen[JsonTest] = unequalJson
      val acceptingStringTests: Gen[StringTest] = equalStrings
      val rejectingStringTests: Gen[StringTest] = unequalString
    }

    object LessThanGens extends NumberComparisonGens(When.LessThan(_)) with UnnestedGens {
      val acceptingJsonTests: Gen[JsonTest] = smallerJson
      val rejectingJsonTests: Gen[JsonTest] = Gen.oneOf(equalJson, largerJson)
      val acceptingStringTests: Gen[StringTest] = smallerStrings
      val rejectingStringTests: Gen[StringTest] = Gen.oneOf(equalStrings, largerStrings)
    }

    object LessThanOrEqualGens extends NumberComparisonGens(When.LessThanOrEqual(_)) with UnnestedGens {
      val acceptingJsonTests: Gen[JsonTest] = Gen.oneOf(equalJson, smallerJson)
      val rejectingJsonTests: Gen[JsonTest] = largerJson
      val acceptingStringTests: Gen[StringTest] = Gen.oneOf(equalStrings, smallerStrings)
      val rejectingStringTests: Gen[StringTest] = largerStrings
    }

    object GreaterThanGens extends NumberComparisonGens(When.GreaterThan(_)) with UnnestedGens {
      val acceptingJsonTests: Gen[JsonTest] = largerJson
      val rejectingJsonTests: Gen[JsonTest] = Gen.oneOf(equalJson, smallerJson)
      val acceptingStringTests: Gen[StringTest] = largerStrings
      val rejectingStringTests: Gen[StringTest] = Gen.oneOf(equalStrings, smallerStrings)
    }

    object GreaterThanOrEqualGens extends NumberComparisonGens(When.GreaterThanOrEqual(_)) with UnnestedGens {
      val acceptingJsonTests: Gen[JsonTest] = Gen.oneOf(equalJson, largerJson)
      val rejectingJsonTests: Gen[JsonTest] = smallerJson
      val acceptingStringTests: Gen[StringTest] = Gen.oneOf(equalStrings, largerStrings)
      val rejectingStringTests: Gen[StringTest] = smallerStrings
    }

    object StartsWithGen extends StringComparisonGens with UnnestedGens {
      def equalRawStrings: Gen[StringTest] =
        jsonGens.stringGen.map {
          case (raw, _) => StringTest(When.StartsWith(raw), raw)
        }

      def rawStringsWithPrefix: Gen[StringTest] =
        for {
          prefix <- rangeGen(0 to 20).flatMap(Gen.stringOfN(_, Gen.alphaNumChar))
          suffix <- rangeGen(1 to 20).flatMap(Gen.stringOfN(_, Gen.alphaNumChar))
        } yield StringTest(When.StartsWith(prefix), s"$prefix$suffix")

      def rawStringsWithoutPrefix: Gen[StringTest] =
        for {
          prefix <- rangeGen(0 to 20).flatMap(Gen.stringOfN(_, Gen.alphaNumChar))
          suffix <- rangeGen(1 to 20).flatMap(Gen.stringOfN(_, Gen.alphaNumChar))
        } yield StringTest(When.StartsWith(s"+$prefix"), s"-$prefix$suffix")

      def equalJsonStrings: Gen[JsonTest] = adaptStringToJsonTest(equalRawStrings)

      def jsonStringsWithPrefix: Gen[JsonTest] = adaptStringToJsonTest(rawStringsWithPrefix)

      def jsonStringsWithoutPrefix: Gen[JsonTest] = adaptStringToJsonTest(rawStringsWithoutPrefix)

      val acceptingJsonTests: Gen[JsonTest] = Gen.oneOf(equalJsonStrings, jsonStringsWithPrefix)
      val rejectingJsonTests: Gen[JsonTest] = jsonStringsWithoutPrefix
      val acceptingStringTests: Gen[StringTest] = Gen.oneOf(equalRawStrings, rawStringsWithPrefix)
      val rejectingStringTests: Gen[StringTest] = rawStringsWithoutPrefix
    }

    object EndsWithGen extends StringComparisonGens with UnnestedGens {
      def equalRawStrings: Gen[StringTest] =
        jsonGens.stringGen.map {
          case (raw, _) => StringTest(When.EndsWith(raw), raw)
        }

      def rawStringsWithSuffix: Gen[StringTest] =
        for {
          prefix <- rangeGen(1 to 20).flatMap(Gen.stringOfN(_, Gen.alphaNumChar))
          suffix <- rangeGen(0 to 20).flatMap(Gen.stringOfN(_, Gen.alphaNumChar))
        } yield StringTest(When.EndsWith(suffix), s"$prefix$suffix")

      def rawStringsWithoutSuffix: Gen[StringTest] =
        for {
          prefix <- rangeGen(1 to 20).flatMap(Gen.stringOfN(_, Gen.alphaNumChar))
          suffix <- rangeGen(0 to 20).flatMap(Gen.stringOfN(_, Gen.alphaNumChar))
        } yield StringTest(When.EndsWith(s"$suffix+"), s"$prefix$suffix-")

      def equalJsonStrings: Gen[JsonTest] = adaptStringToJsonTest(equalRawStrings)

      def jsonStringsWithSuffix: Gen[JsonTest] = adaptStringToJsonTest(rawStringsWithSuffix)

      def jsonStringsWithoutSuffix: Gen[JsonTest] = adaptStringToJsonTest(rawStringsWithoutSuffix)

      val acceptingJsonTests: Gen[JsonTest] = Gen.oneOf(equalJsonStrings, jsonStringsWithSuffix)
      val rejectingJsonTests: Gen[JsonTest] = jsonStringsWithoutSuffix
      val acceptingStringTests: Gen[StringTest] = Gen.oneOf(equalRawStrings, rawStringsWithSuffix)
      val rejectingStringTests: Gen[StringTest] = rawStringsWithoutSuffix
    }

    object ContainsGen extends StringComparisonGens with UnnestedGens {
      def equalRawStrings: Gen[StringTest] =
        jsonGens.stringGen.map {
          case (raw, _) => StringTest(When.Contains(raw), raw)
        }

      def rawStringsWithSubstring: Gen[StringTest] =
        for {
          prefix <- rangeGen(1 to 20).flatMap(Gen.stringOfN(_, Gen.alphaNumChar))
          subString <- rangeGen(0 to 20).flatMap(Gen.stringOfN(_, Gen.alphaNumChar))
          suffix <- rangeGen(1 to 20).flatMap(Gen.stringOfN(_, Gen.alphaNumChar))
        } yield StringTest(When.Contains(subString), s"$prefix$subString$suffix")

      def rawStringsWithoutSubstring: Gen[StringTest] =
        for {
          prefix <- rangeGen(1 to 20).flatMap(Gen.stringOfN(_, Gen.alphaNumChar))
          suffix <- rangeGen(0 to 20).flatMap(Gen.stringOfN(_, Gen.alphaNumChar))
        } yield StringTest(When.Contains(s"+$suffix+"), s"-$prefix-$suffix-")

      def equalJsonStrings: Gen[JsonTest] = adaptStringToJsonTest(equalRawStrings)

      def jsonStringsWithSubstring: Gen[JsonTest] = adaptStringToJsonTest(rawStringsWithSubstring)

      def jsonStringsWithoutSubstring: Gen[JsonTest] = adaptStringToJsonTest(rawStringsWithoutSubstring)

      val acceptingJsonTests: Gen[JsonTest] = Gen.oneOf(equalJsonStrings, jsonStringsWithSubstring)
      val rejectingJsonTests: Gen[JsonTest] = jsonStringsWithoutSubstring
      val acceptingStringTests: Gen[StringTest] = Gen.oneOf(equalRawStrings, rawStringsWithSubstring)
      val rejectingStringTests: Gen[StringTest] = rawStringsWithoutSubstring
    }

    override def values: IndexedSeq[UnnestedGens] = findValues

    def acceptingJsonTests: Gen[JsonTest] = Gen.oneOf(
      UnnestedGens.values.map(_.acceptingJsonTests)
    ).flatMap(identity)

    def rejectingJsonTests: Gen[JsonTest] = Gen.oneOf(
      UnnestedGens.values.map(_.rejectingJsonTests)
    ).flatMap(identity)

    def acceptingStringTests: Gen[StringTest] = Gen.oneOf(
      UnnestedGens.values.map(_.acceptingStringTests)
    ).flatMap(identity)

    def rejectingStringTests: Gen[StringTest] = Gen.oneOf(
      UnnestedGens.values.map(_.rejectingStringTests)
    ).flatMap(identity)
  }

  object NestedGens {
    object NotGens {
      private def wrap(test: JsonTest): JsonTest = test.copy(when = When.Not(test.when))

      private def wrap(test: StringTest): StringTest = test.copy(when = When.Not(test.when))

      val innerWhenAcceptsJson: Gen[JsonTest] = UnnestedGens.acceptingJsonTests.map(wrap)
      val innerWhenRejectsJson: Gen[JsonTest] = UnnestedGens.rejectingJsonTests.map(wrap)
      val innerWhenAcceptsStrings: Gen[StringTest] = UnnestedGens.acceptingStringTests.map(wrap)
      val innerWhenRejectsStrings: Gen[StringTest] = UnnestedGens.rejectingStringTests.map(wrap)
    }

    object AtGens {
      private val paths: Gen[(JsonPath, Json => Json)] = {
        val downN: Gen[(CursorOp, String, Json => Json)] =
          for {
            before <- rangeGen(1 to 10).map(List.range(0, _).map(i => s"p$i".asJson))
            after <- rangeGen(1 to 5).map(List.range(0, _).map(i => s"s$i".asJson))
          } yield {
            val wrapper: Json => Json = j => (before ::: (j :: after)).asJson
            (CursorOp.DownN(before.length), s"[${before.length}]", wrapper)
          }

        val downField: Gen[(CursorOp, String, Json => Json)] = {
          def fields(i: Int, tag: String): Gen[(String, Json)] = jsonGens.scalarGen.map(s"$i$tag" -> _)

          def fillFields(count: Int, tag: String): Gen[List[(String, Json)]] =
            Gen.sequence(List.range(0, count).map(fields(_, tag))).map(_.asScala.toList)

          for {
            target <- rangeGen(1 to 20).flatMap(Gen.stringOfN(_, Gen.alphaChar))
            before <- rangeGen(1 to 10).flatMap(fillFields(_, "p"))
            after <- rangeGen(1 to 5).flatMap(fillFields(_, "s"))
          } yield {
            val wrapper: Json => Json = j => Json.fromFields(before ::: ((target, j) :: after))

            (CursorOp.DownField(target), target, wrapper)
          }
        }

        rangeGen(1 to 5).flatMap { depth =>
          Gen.listOfN(depth, Gen.oneOf(downN, downField)).map { layers =>
            val (ops, names, wrappers) = layers.unzip3
            JsonPath(ops, names.mkString(".", ".", "")) -> wrappers.foldLeft(identity[Json](_))(_.andThen(_))
          }
        }
      }

      private def wrap(test: JsonTest): Gen[JsonTest] = paths.map {
        case (path, wrapper) => JsonTest(When.At(path, test.when), wrapper(test.input))
      }

      val innerWhenAcceptsJson: Gen[JsonTest] = UnnestedGens.acceptingJsonTests.flatMap(wrap)
      val innerWhenRejectsJson: Gen[JsonTest] = UnnestedGens.rejectingJsonTests.flatMap(wrap)
      val pathDoesNotExistInJson: Gen[JsonTest] = UnnestedGens.acceptingJsonTests.flatMap { test =>
        for {
          actualPathTest <- wrap(test)
          fakePathTest <- wrap(test)
        } yield actualPathTest.copy(when = fakePathTest.when)
      }
    }

    object AnyGens {
      val scalarJson: Gen[JsonTest] = UnnestedGens.acceptingJsonTests.map { test =>
        test.copy(when = When.Any(test.when))
      }

      val arrayIsEmpty: Gen[JsonTest] = UnnestedGens.acceptingJsonTests.map(_.copy(input = Json.arr()))

      val objIsEmpty: Gen[JsonTest] = UnnestedGens.acceptingJsonTests.map(_.copy(input = Json.obj()))

      val noElementsMatch: Gen[JsonTest] =
        for {
          test <- UnnestedGens.rejectingJsonTests
          elements <- rangeGen(1 to 10).map(List.fill(_)(test.input))
        } yield JsonTest(When.Any(test.when), elements.asJson)

      val noValuesMatch: Gen[JsonTest] =
        for {
          test <- UnnestedGens.rejectingJsonTests
          elements <- rangeGen(1 to 10).map(List.range(0, _).map(i => s"f$i" -> test.input))
        } yield JsonTest(When.Any(test.when), Json.fromFields(elements))

      val oneElementMatches: Gen[JsonTest] = UnnestedGens.acceptingJsonTests.flatMap { test =>
        for {
          before <- rangeGen(1 to 10).map(List.range(0, _).map(i => s"p$i".asJson))
          after <- rangeGen(1 to 5).map(List.range(0, _).map(i => s"s$i".asJson))
        } yield JsonTest(When.Any(test.when), (before ::: (test.input :: after)).asJson)
      }

      val oneValueMatches: Gen[JsonTest] = UnnestedGens.acceptingJsonTests.flatMap { test =>
        def fields(i: Int, tag: String): Gen[(String, Json)] = jsonGens.scalarGen.map(s"$i$tag" -> _)

        def fillFields(count: Int, tag: String): Gen[List[(String, Json)]] =
          Gen.sequence(List.range(0, count).map(fields(_, tag))).map(_.asScala.toList)

        for {
          before <- rangeGen(1 to 10).flatMap(fillFields(_, "p"))
          after <- rangeGen(1 to 5).flatMap(fillFields(_, "s"))
        } yield JsonTest(When.Any(test.when), Json.fromFields(before ::: (("target" -> test.input) :: after)))
      }

      val allElementsMatch: Gen[JsonTest] = UnnestedGens.acceptingJsonTests.flatMap { test =>
        for {
          elements <- rangeGen(1 to 10).map(List.fill(_)(test.input))
        } yield JsonTest(When.Any(test.when), elements.asJson)
      }

      val allValuesMatch: Gen[JsonTest] =
        for {
          test <- UnnestedGens.acceptingJsonTests
          elements <- rangeGen(1 to 10).map(List.range(0, _).map(i => s"f$i" -> test.input))
        } yield JsonTest(When.Any(test.when), Json.fromFields(elements))
    }

    abstract class ExistsAndForAllGens[W <: When](wrap: NonEmptyList[When] => W) {
      val jsonNoNestedWhensPasses: Gen[JsonTest] = {
        val nonNumericGens = Gen.oneOf(
          jsonGens.boolGen, jsonGens.nullGen, jsonGens.stringGen.map(_._2)
        )
        for {
          nonMatchingHead <- nonNumericGens
          nonMatchingTail <- rangeGen(0 to 20).flatMap(Gen.listOfN(_, nonNumericGens))
          number <- jsonGens.numberGen.map(_._2)
        } yield JsonTest(wrap(NonEmptyList(nonMatchingHead, nonMatchingTail).map(When.Equal(_))), number)
      }

      val stringNoNestedWhensPasses: Gen[StringTest] = {
        val nonNumericGens = Gen.oneOf(
          jsonGens.boolGen, jsonGens.nullGen, jsonGens.numberGen.map(_._2)
        )
        for {
          nonMatchingHead <- nonNumericGens
          nonMatchingTail <- rangeGen(0 to 20).flatMap(Gen.listOfN(_, nonNumericGens))
          text <- jsonGens.stringGen.map(_._1)
        } yield StringTest(wrap(NonEmptyList(nonMatchingHead, nonMatchingTail).map(When.Equal(_))), text)
      }

      val jsonOneNestedWhenPasses: Gen[JsonTest] = {
        val nonNumericGens = Gen.oneOf(
          jsonGens.boolGen, jsonGens.nullGen, jsonGens.numberGen.map(_._2)
        )
        for {
          (_, input) <- jsonGens.numberGen
          rejectingTests <- rangeGen(0 to 20).flatMap(Gen.listOfN(_, nonNumericGens)).map {
            _.map(When.Equal(_))
          }
        } yield JsonTest(wrap(NonEmptyList(When.Equal(input), rejectingTests)), input)
      }

      val stringOneNestedWhenPasses: Gen[StringTest] = {
        val nonNumericGens = Gen.oneOf(
          jsonGens.boolGen, jsonGens.nullGen, jsonGens.numberGen.map(_._2)
        )
        for {
          (input, json) <- jsonGens.stringGen
          rejectingTests <- rangeGen(0 to 20).flatMap(Gen.listOfN(_, nonNumericGens)).map {
            _.map(When.Equal(_))
          }
        } yield StringTest(wrap(NonEmptyList(When.Equal(json), rejectingTests)), input)
      }

      val jsonAtLeastOneNestedWhenPasses: Gen[JsonTest] = {
        val nonNumericGens = Gen.oneOf(
          jsonGens.boolGen, jsonGens.nullGen, jsonGens.stringGen.map(_._2)
        )
        for {
          (_, input) <- jsonGens.numberGen
          acceptingTests <- rangeGen(1 to 20).map(List.fill(_)(When.Equal(input)))
          rejectingTests <- rangeGen(1 to 20).flatMap(Gen.listOfN(_, nonNumericGens)).map {
            _.map(When.Equal(_))
          }
        } yield JsonTest(
          wrap(NonEmptyList(When.Equal(input), rejectingTests).concat(acceptingTests)),
          input
        )
      }

      val stringAtLeastOneNestedWhenPasses: Gen[StringTest] = {
        val nonStringGens = Gen.oneOf(
          jsonGens.boolGen, jsonGens.nullGen, jsonGens.numberGen.map(_._2)
        )
        for {
          (input, json) <- jsonGens.stringGen
          acceptingTests <- rangeGen(1 to 20).map(List.fill(_)(When.Equal(json)))
          rejectingTests <- rangeGen(1 to 20).flatMap(Gen.listOfN(_, nonStringGens)).map {
            _.map(When.Equal(_))
          }
        } yield StringTest(
          wrap(NonEmptyList(When.Equal(json), rejectingTests).concat(acceptingTests)),
          input
        )
      }

      val jsonAllNestedWhensPass: Gen[JsonTest] =
        Gen.oneOf(
          for {
            len <- rangeGen(0 to 20)
            test <- UnnestedGens.EqualityGens.acceptingJsonTests
          } yield JsonTest(wrap(NonEmptyList(test.when, List.fill(len)(test.when))), test.input),
          for {
            len <- rangeGen(1 to 10)
            testsHead <- ContainsGen.rawStringsWithSubstring
            testsTail <- Gen.sequence(List.fill(len)(ContainsGen.rawStringsWithSubstring)).map(_.asScala.toList)
          } yield JsonTest(
            wrap(NonEmptyList(testsHead.when, testsTail.map(_.when))),
            testsTail.map(_.input).mkString(testsHead.input, "", "").asJson
          )
        )

      val stringAllNestedWhensPass: Gen[StringTest] =
        Gen.oneOf(
          for {
            len <- rangeGen(0 to 20)
            test <- UnnestedGens.EqualityGens.acceptingStringTests
          } yield StringTest(wrap(NonEmptyList(test.when, List.fill(len)(test.when))), test.input),
          for {
            len <- rangeGen(1 to 10)
            testsHead <- ContainsGen.rawStringsWithSubstring
            testsTail <- Gen.sequence(List.fill(len)(ContainsGen.rawStringsWithSubstring)).map(_.asScala.toList)
          } yield StringTest(
            wrap(NonEmptyList(testsHead.when, testsTail.map(_.when))),
            testsTail.map(_.input).mkString(testsHead.input, "", "")
          )
        )
    }

    object ExistsGens extends ExistsAndForAllGens(When.Exists(_))

    object ForallGens extends ExistsAndForAllGens(When.Forall(_))
  }
}