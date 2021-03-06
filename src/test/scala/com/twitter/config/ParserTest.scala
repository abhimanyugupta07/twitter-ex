package com.twitter.config

import com.twitter.config.adt._
import com.twitter.config.parser.ConfigLoader
import fastparse.core.Parsed
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers, OptionValues}

class ParserTest extends FlatSpec with Matchers with OptionValues with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSize = 100, maxSize = 100)

  val asciiString = Gen.containerOf[Array, Char](Gen.alphaNumChar).map(_.mkString)
  val asciiSeq = Gen.listOf(asciiString)

  implicit class ParsedAugmenter[T](val result: Parsed[T]) {
    def option: Option[T] = result match {
      case Parsed.Success(res, index) => Some(res)
      case Parsed.Failure(last, index, extra) => None
    }
  }

  def noDelimiters(str: String): Boolean = {
    val delimiters = List('\n', '\r', ';', ',')
    delimiters.forall(!str.contains(_))
  }

  def validString(str: String): Boolean = {
    str.nonEmpty && str.trim.nonEmpty && """[\;,\,,\n,\r]""".r.findFirstIn(str).isEmpty
  }

  def validOverrideString(str: String): Boolean = {
    str.nonEmpty && str.trim.nonEmpty && """\<,\>,\=,\,,\n,\r]""".r.findFirstIn(str).isEmpty
  }


  val loader = new ConfigLoader()

  it should "correctly parse a group definition" in {
    forAll(asciiString) { s: String =>
      val group = s"[$s]"

      whenever(s.length > 0 && s.indexOf(']') == -1) {
        val parsed = loader.groupParser.parse(group).option
        parsed shouldBe defined
        parsed.value.value shouldEqual Group(group.drop(1).dropRight(1))
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

  "The list parser" should "correctly parse a list of numeric long values list from a generator" in {
    forAll { numbers: Seq[Long] =>
      if (numbers.size > 1) {
        val parsed = loader.numberList.parse(numbers.mkString(",")).option
        parsed shouldBe defined

        numbers.size shouldEqual parsed.value.value.size

        parsed.value.value should contain theSameElementsAs numbers
      } else {
        val parsed = loader.numberList.parse(numbers.mkString(",")).option
        parsed shouldBe empty

      }
    }
  }

  it should "correctly parse comment blocks" in {
    val comment = ";this is a nice comment\n"

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

  "The string list parser" should "parse a generated entry to a list of strings" in {
    forAll(asciiSeq) { strings =>
      if (strings.length > 1 && strings.forall(validString)) {
        val input = strings.mkString(",")
        val parsed = loader.stringList.parse(input).option

        parsed shouldBe defined
        parsed.value.value should contain theSameElementsAs strings
      } else {
        val input = strings.mkString(",")
        val parsed = loader.stringList.parse(input).option

        parsed shouldBe empty
      }
    }
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
    parsed.value._2.value shouldBe SettingOverride(overrideGroup)
  }

  "The setting key parser" should "parse a generated setting key without an override" in {
    forAll(asciiString) { str =>
      if (validOverrideString(str)) {
        val parsed = loader.settingKeyParser.parse(str).option
        parsed shouldBe defined
        parsed.value._1 shouldEqual str
        parsed.value._2 shouldBe empty
      } else {
        val parsed = loader.settingKeyParser.parse(str).option
        parsed shouldBe empty
      }
    }
  }

  "The setting key parser" should "parse a generated setting key with an override" in {
    forAll(asciiString, asciiString) { (str, ov) =>
      val setting = s"$str<$ov>"

      if (validOverrideString(str) && validOverrideString(ov)) {
        val parsed = loader.settingKeyParser.parse(setting).option
        parsed shouldBe defined
        parsed.value._1 shouldEqual str

        parsed.value._2 shouldBe defined
        parsed.value._2.value shouldBe SettingOverride(ov)
      } else {
        val parsed = loader.settingKeyParser.parse(setting).option
        parsed shouldBe empty
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

  "The value parser" should "parse a long value from a numeric input" in {
    forAll { (num: Long) =>
      val parsed = loader.valueParser.parse(num.toString).option
      parsed shouldBe defined
      parsed.value.value shouldEqual num
      parsed.value.isInstanceOf[LongValue] shouldEqual true
    }
  }

  def nonDelimitedStr(str: String): Boolean = {
    str.indexOf(loader.eol) == -1 && str.indexOf('\n') == -1 && str.indexOf('\r') == -1
  }

  val inputs = Gen.nonEmptyListOf(Gen.uuid.map(_.toString))

  "The value parser" should "parse a string list value from an input" in {
    forAll(inputs) { strings =>
      if (strings.size > 1 && strings.forall(_.nonEmpty) && strings.forall(noDelimiters)) {
        val input = strings.mkString(",")
        val parsed = loader.valueParser.parse(input).option
        parsed shouldBe defined
        parsed.value.isInstanceOf[StringListValue] shouldEqual true
        parsed.value.asInstanceOf[StringListValue].value should contain theSameElementsAs strings
      } else if (strings.size == 1) {
        val input = strings.mkString(",")
        val parsed = loader.valueParser.parse(input).option
        parsed shouldBe defined

        parsed.value.isInstanceOf[StringValue] shouldEqual true
        parsed.value.asInstanceOf[StringValue].value shouldEqual strings.head
      } else if (strings.isEmpty) {
        val input = strings.mkString(",")
        val parsed = loader.valueParser.parse(input).option
        parsed shouldBe empty
      }
    }
  }

  "The setting parser" should "parse a boolean setting without an override and with spaces" in {
    val key = "key"
    val value = "true"
    val str = s"$key = $value"
    val parsed = loader.settingParser.parse(str).option

    parsed shouldBe defined
    parsed.value.setting.key shouldEqual key
    parsed.value.setting.envOverride shouldBe empty
  }

  "The setting parser" should "parse a long setting without an override and with spaces" in {
    val key = "key"
    val value = 150L
    val str = s"$key = $value"
    val parsed = loader.settingParser.parse(str).option

    parsed shouldBe defined
    parsed.value.setting.key shouldEqual key
    parsed.value.setting.value.isInstanceOf[LongValue] shouldEqual true
    parsed.value.setting.value.asInstanceOf[LongValue].value shouldEqual value

    parsed.value.setting.envOverride shouldBe empty
  }

  "The setting parser" should "parse a long setting with an override and with spaces" in {
    val key = "key"
    val overrideGroup = "override"
    val value = 150L
    val str = s"$key<$overrideGroup> = $value"
    val parsed = loader.settingParser.parse(str).option

    parsed shouldBe defined
    parsed.value.setting.key shouldEqual key
    parsed.value.setting.value.isInstanceOf[LongValue] shouldEqual true
    parsed.value.setting.value.asInstanceOf[LongValue].value shouldEqual value

    parsed.value.setting.envOverride shouldBe defined
    parsed.value.setting.envOverride.value shouldBe SettingOverride(overrideGroup)
  }

  "The setting parser" should "parse a boolean setting without an override and without spaces" in {
    val key = "key"
    val value = "true"
    val str = s"$key=$value"
    val parsed = loader.settingParser.parse(str).option

    parsed shouldBe defined
    parsed.value.setting.key shouldEqual key
    parsed.value.setting.envOverride shouldBe empty
  }

  "The setting parser" should "parse a boolean setting with an override and without spaces" in {
    val key = "key"
    val overrideGroup = "override"
    val value = "true"
    val str = s"$key<$overrideGroup>=$value"
    val parsed = loader.settingParser.parse(str).option

    parsed shouldBe defined
    parsed.value.setting.key shouldEqual key
    parsed.value.setting.envOverride shouldBe defined
    parsed.value.setting.envOverride.value shouldBe SettingOverride(overrideGroup)
  }


  "The setting parser" should "parse a boolean setting with an override and with spaces" in {
    val key = "key"
    val overrideGroup = "override"
    val value = "true"
    val str = s"$key<$overrideGroup> = $value"
    val parsed = loader.settingParser.parse(str).option

    parsed shouldBe defined
    parsed.value.setting.key shouldEqual key

    parsed.value.setting.value.isInstanceOf[BooleanConfigValue] shouldEqual true
    parsed.value.setting.value.asInstanceOf[BooleanConfigValue].value shouldEqual true

    parsed.value.setting.envOverride shouldBe defined
    parsed.value.setting.envOverride.value shouldBe SettingOverride(overrideGroup)
  }

  "The setting parser" should "parse a string list with an override" in {
    forAll(asciiSeq) { seq =>
      if (seq.size > 1 && seq.forall(validString)) {
        val key = "key"
        val overrideGroup = "override"
        val str = s"$key<$overrideGroup>=${seq.mkString(",")}"

        val parsed = loader.settingParser.parse(str).option

        parsed shouldBe defined
        parsed.value.setting.key shouldEqual key

        parsed.value.setting.value.isInstanceOf[StringListValue] shouldEqual true
        parsed.value.setting.value.asInstanceOf[StringListValue].value should contain theSameElementsAs seq

        parsed.value.setting.envOverride shouldBe defined
        parsed.value.setting.envOverride.value shouldBe SettingOverride(overrideGroup)
      }
    }
  }

  "The setting parser" should "parse a string list without an override" in {
    forAll(asciiSeq) { seq =>
      if (seq.size > 1 && seq.forall(validString)) {
        val key = "key"
        val str = s"$key=${seq.mkString(",")}"

        val parsed = loader.settingParser.parse(str).option

        parsed shouldBe defined
        parsed.value.setting.key shouldEqual key

        parsed.value.setting.value.isInstanceOf[StringListValue] shouldEqual true
        parsed.value.setting.value.asInstanceOf[StringListValue].value should contain theSameElementsAs seq

        parsed.value.setting.envOverride shouldBe empty
      }
    }
  }

  "The setting parser" should "parse a numeric list with an override" in {
    forAll { seq: Seq[Long] =>
      if (seq.size > 1) {
        val key = "key"
        val overrideGroup = "override"
        val str = s"$key<$overrideGroup>=${seq.mkString(",")}"

        val parsed = loader.settingParser.parse(str).option

        parsed shouldBe defined
        parsed.value.setting.key shouldEqual key

        parsed.value.setting.value.isInstanceOf[NumericListValue] shouldEqual true
        parsed.value.setting.value.asInstanceOf[NumericListValue].value should contain theSameElementsAs seq

        parsed.value.setting.envOverride shouldBe defined
        parsed.value.setting.envOverride.value shouldBe SettingOverride(overrideGroup)
      }
    }
  }

  "The setting parser" should "parse a numeric list without an override" in {
    forAll { seq: Seq[Long] =>
      if (seq.size > 1) {
        val key = "key"
        val str = s"$key=${seq.mkString(",")}"

        val parsed = loader.settingParser.parse(str).option

        parsed shouldBe defined
        parsed.value.setting.key shouldEqual key

        parsed.value.setting.value.isInstanceOf[NumericListValue] shouldEqual true
        parsed.value.setting.value.asInstanceOf[NumericListValue].value should contain theSameElementsAs seq

        parsed.value.setting.envOverride shouldBe empty
      }
    }
  }

  "The setting parser" should "parse a quoted string with an override" in {
    val key = "key"
    val overrideGroup = "override"
    val innerValue = "this, is really nice"
    val value = "\"" + innerValue + "\""
    val str = s"$key<$overrideGroup>=$value"
    val parsed = loader.settingParser.parse(str).option

    parsed shouldBe defined
    parsed.value.setting.key shouldEqual key

    parsed.value.setting.value.isInstanceOf[StringValue] shouldEqual true
    parsed.value.setting.value.asInstanceOf[StringValue].value shouldEqual innerValue

    parsed.value.setting.envOverride shouldBe defined
    parsed.value.setting.envOverride.value shouldBe SettingOverride(overrideGroup)
  }

  "The setting parser" should "parse a quoted string without an override" in {
    val key = "key"
    val innerValue = "this, is really nice"
    val value = "\"" + innerValue + "\""
    val str = s"$key=$value"
    val parsed = loader.settingParser.parse(str).option

    parsed shouldBe defined
    parsed.value.setting.key shouldEqual key

    parsed.value.setting.value.isInstanceOf[StringValue] shouldEqual true
    parsed.value.setting.value.asInstanceOf[StringValue].value shouldEqual innerValue

    parsed.value.setting.envOverride shouldBe empty
  }

}
