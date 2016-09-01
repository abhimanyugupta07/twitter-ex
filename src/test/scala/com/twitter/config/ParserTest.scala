package com.twitter.config

import com.twitter.config.adt.BooleanConfigValue
import com.twitter.config.parser.ConfigLoader
import fastparse.core.Parsed
import org.scalatest.prop.Configuration.PropertyCheckConfig
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers, OptionValues}

class ParserTest extends FlatSpec with Matchers with OptionValues with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSize = 500, maxSize = 500)

  implicit class ParsedAugmenter[T](val result: Parsed[T]) {
    def option: Option[T] = result match {
      case Parsed.Success(res, index) => Some(res)
      case Parsed.Failure(last, index, extra) => None
    }
  }

  val loader = new ConfigLoader()

  it should "correctly parse a group definition" in {
    forAll { s: String =>
      val group = s"[$s]"

      whenever(s.length > 0 && s.indexOf(']') == -1) {
        val parsed = loader.groupParser.parse(group).option
        parsed shouldBe defined
        parsed.value.value shouldEqual Group(group)
      }
    }
  }

  "The long parser" should "parse a numeric value from a string without a sign" in {

    val num = 235789375892375L

    val parsed = loader.numParser.parse(num.toString).option
    parsed shouldBe defined
    parsed.value.value shouldEqual num
  }

  "The long parser" should "parse a numeric value from a string with a plus sign" in {

    val num = 235789375892375L

    val parsed = loader.numParser.parse("+" + num.toString).option
    parsed shouldBe defined
    parsed.value.value shouldEqual num
  }

  "The long parser" should "parse a numeric value from a string with a minus sign" in {
    val num = 235789375892375L

    val parsed = loader.numParser.parse("-" + num.toString).option
    parsed shouldBe defined
    parsed.value.value shouldEqual -num
  }

  "The long parser" should "correctly parse a numeric value" in {
    forAll { num: Long =>
      val parsed = loader.numParser.parse(num.toString).option
      parsed shouldBe defined
      parsed.value.value shouldEqual num
    }
  }

  "The list parser" should "correctly parse a list of numeric values from a single input" in {
    val numbers = List(125125L, 1251251L)

    val parsed = loader.numberList.parse(numbers.mkString(",")).option

    parsed shouldBe defined

    numbers.size shouldEqual parsed.value.value.size

    parsed.value.value should contain theSameElementsAs numbers
  }

  "The list parser" should "correctly parse a list of numeric long values list from a generator" ignore {
    forAll { numbers: Seq[Long] =>
      whenever(numbers.nonEmpty) {
        val parsed = loader.numberList.parse(numbers.mkString(",")).option
        parsed shouldBe defined

        Console.println(s"INPUT: ${numbers.mkString(" ")} OUTPUT: ${parsed.value.value.mkString(" ")}")

        numbers.size shouldEqual parsed.value.value.size

        parsed.value.value should contain theSameElementsAs numbers
      }

    }
  }

  it should "correctly parse comment blocks" in {
    val comment = ";this is a nice comment\n"

    val parsed = loader.commentParser.parse(comment).option
    parsed shouldBe defined

    parsed.value.comment shouldEqual "this is a nice comment"
  }

  it should "correctly parse comment blocks from multi-line definitions" in {
    val comment =
      """;this is a nice comment
        | isn't it
        | """".stripMargin

    val parsed = loader.commentParser.parse(comment).option
    parsed shouldBe defined

    parsed.value.comment shouldEqual "this is a nice comment"

  }

  "The true parser" should "parse a true value from the strings true and yes" in {
    val parsed = loader.trueParser.parse("true").option
    parsed shouldBe defined
    parsed.value shouldEqual true

    val parsed2 = loader.trueParser.parse("yes").option
    parsed2 shouldBe defined
    parsed2.value shouldEqual true
  }

  "The false parser" should "parse a false value from the strings false and no" in {
    val parsed = loader.falseParser.parse("false").option
    parsed shouldBe defined
    parsed.value shouldEqual false

    val parsed2 = loader.falseParser.parse("no").option
    parsed2 shouldBe defined
    parsed2.value shouldEqual false
  }

  "The boolean parser" should "parse a true value from the strings true and yes" in {
    val parsed = loader.booleanParser.parse("true").option
    parsed shouldBe defined
    parsed.value.value shouldEqual true

    val parsed2 = loader.booleanParser.parse("yes").option
    parsed2 shouldBe defined
    parsed2.value.value shouldEqual true
  }

  "The boolean parser" should "parse a false value from the strings false and no" in {
    val parsed = loader.booleanParser.parse("false").option
    parsed shouldBe defined
    parsed.value.value shouldEqual false

    val parsed2 = loader.booleanParser.parse("no").option
    parsed2 shouldBe defined
    parsed2.value.value shouldEqual false
  }

  "The string parser" should "parse a simple string without delimiters" in {
    val str = "value"

    val parsed = loader.strParser.parse(str).option

    parsed shouldBe defined
    parsed.value.value shouldEqual "value"
  }

  "The string parser" should "parse a simple string and ignore a comment" in {
    val str = "value;comment"

    val parsed = loader.strParser.parse(str).option

    parsed shouldBe defined
    parsed.value.value shouldEqual "value"
  }

  "The string list parser" should "parse a single entry in a comma delimited list of strings" in {
    val input = "test,test2,test4"

    val parsed = loader.stringList.parse(input).option
    parsed shouldBe defined
    parsed.value.value should contain theSameElementsAs List("test", "test2", "test4")
  }

  "The setting key parser" should "parse a single setting key without an override" in {
    val str = "testsetting"
    val parsed = loader.settingKeyParser.parse(str).option

    parsed shouldBe defined
    parsed.value._1 shouldEqual str
    parsed.value._2 shouldBe empty
  }

  "The setting key parser" should "parse a single setting key with an override" in {
    val key = "key"
    val overrideGroup = "override"
    val str = s"$key<$overrideGroup>"
    val parsed = loader.settingKeyParser.parse(str).option

    parsed shouldBe defined
    parsed.value._1 shouldEqual key
    parsed.value._2 shouldBe defined
    parsed.value._2.value shouldBe Group(overrideGroup)
  }

  "The setting key parser" should "parse a setting key without an override" in {
    forAll { str: String =>
      whenever(str.indexOf('<') == -1 && str.indexOf('>') == -1) {
        val parsed = loader.settingKeyParser.parse(str).option
        parsed shouldBe defined
        parsed.value._1 shouldEqual str
        parsed.value._2 shouldBe empty
      }
    }
  }

  "The setting key parser" should "parse a setting key with an override" in {
    forAll { (str: String, ov: String) =>
      val setting = s"$str<$ov>"

      whenever(str.indexOf('<') == -1 &&
        str.indexOf('>') == -1 &&
        ov.indexOf('<') == -1 &&
        ov.indexOf('>') == -1
      ) {
        val parsed = loader.settingKeyParser.parse(setting).option
        parsed shouldBe defined
        parsed.value._1 shouldEqual str

        parsed.value._2 shouldBe defined
        parsed.value._2.value shouldBe Group(ov)
      }
    }
  }

  "The value parser" should "parser a boolean configuration option from true" in {
    val value = "true"
    val parsed = loader.valueParser.parse(value).option

    parsed shouldBe defined
    parsed.value.value shouldEqual true
    parsed.value.isInstanceOf[BooleanConfigValue] shouldEqual true
  }

  "The value parser" should "parser a boolean configuration option from yes" in {
    val value = "yes"
    val parsed = loader.valueParser.parse(value).option

    parsed shouldBe defined
    parsed.value.value shouldEqual true
    parsed.value.isInstanceOf[BooleanConfigValue] shouldEqual true
  }


  "The value parser" should "parser a boolean configuration option from false" in {
    val value = "false"
    val parsed = loader.valueParser.parse(value).option

    parsed shouldBe defined
    parsed.value.value shouldEqual false
    parsed.value.isInstanceOf[BooleanConfigValue] shouldEqual true
  }

  "The value parser" should "parser a boolean configuration option from no" in {
    val value = "no"
    val parsed = loader.valueParser.parse(value).option

    parsed shouldBe defined
    parsed.value.value shouldEqual false
    parsed.value.isInstanceOf[BooleanConfigValue] shouldEqual true
  }


  "The setting parser" should "parse a boolean setting with an override" ignore {
    val key = "key"
    val overrideGroup = "override"
    val value = "true"
    val str = s"$key<$overrideGroup> = $value"
    val parsed = loader.settingParser.parse(str).option

    parsed shouldBe defined
    parsed.value.setting.key shouldEqual key
    parsed.value.setting.group shouldBe defined
    parsed.value.setting.group.value shouldBe Group(overrideGroup)
  }
}
