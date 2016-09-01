package com.twitter.config.parser

import com.twitter.config.adt._
import com.twitter.config.{Config, Group}
import scala.io.Source
import fastparse.all._

trait ParsedLine

object EmptyLine extends ParsedLine

case class ParsedComment(comment: String) extends ParsedLine

case class ParsedSettingValue[T](setting: SettingValue[T]) extends ParsedLine

case class ParsedGroup(value: Group) extends ParsedLine

case class OrphanedLine(value: String) extends ParsedLine

class ConfigLoader {
  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V){
    def apply(t: T) = f(t)
    override def toString() = name
  }

  val eol = sys.props.get("line.separator").getOrElse(throw new RuntimeException("Could not get line separator"))
  val eoz = "\\z"

  val Whitespace = NamedFunction(" \r\n".contains(_: Char), "Whitespace")
  val Digits = NamedFunction('0' to '9' contains (_: Char), "Digits")
  val NonLineEnding = NamedFunction(!"\r\n".contains(_: Char), "StringChars")
  val nonDelimited = NamedFunction(!",\r\n".contains(_: Char), "StringChars")
  val NonWhitespace = NamedFunction(!" \r\n".contains(_: Char), "StringChars")

  val groupParser: P[ParsedGroup] = P(("[" ~/ CharsWhile(_ != ']').! ~/ "]").!.map(str => ParsedGroup(Group(str))))

  val overrideParser: P[Group] = P("<" ~/ CharsWhile(_ != '>').! ~/ ">").map(Group)

  val signParser: P[String] = P(Start ~ ("-" | "+").!)

  val digitParser = P(signParser.? ~ CharIn('0' to '9').rep(1).!)

  val rawLongParser: P[Long] = P(signParser.? ~ CharIn('0' to '9').rep(1)).!.map(_.toLong)

  val trueParser = P(("true" | "yes").!).map(_ => true)
  val falseParser = P(("false" | "no").!).map(_ => false)

  val booleanParser: P[BooleanConfigValue] = P(trueParser | falseParser).map(BooleanConfigValue)

  /**
    * Numeric parser for Long should check Long overflow.
    */
  val numParser: P[LongValue] = rawLongParser.map(LongValue)

  val space  = P(CharIn(Seq('\r', '\n', ' ')).?)

  val numberList: P[NumericListValue] = P(rawLongParser.rep(sep = ",".?)) map NumericListValue

  val strParser: P[StringValue] = P(CharsWhile(!";\r\n".contains(_)).!) map StringValue

  val stringListRaw: P[Seq[String]] = P(CharsWhile(nonDelimited).!).rep(sep = ",")

  val stringList: P[StringListValue] = P(CharsWhile(nonDelimited).!).rep(sep = ",").map(StringListValue)

  val valueParser: P[ConfigValue[_]] = P(booleanParser | numParser | numberList | stringList | strParser)

  val settingKeyParser = P(CharsWhile(_ != '<').rep.! ~ overrideParser.?)

  val settingParser: P[ParsedSettingValue[_]] = P(settingKeyParser ~/ space.rep.? ~/ "=" ~/ space.rep.? ~/ valueParser).map {
    case (key, groupOverride, value) => {
      ParsedSettingValue(SettingValue(key, value, groupOverride))
    }
  }

  val commentParser: P[ParsedComment] = P(";" ~/ CharsWhile(NonLineEnding).rep.! ~/ CharsWhile(Whitespace).rep(1)).map(ParsedComment)

  val orphanedLine: P[OrphanedLine] = P(CharsWhile(!";\r\n".contains(_)).rep.!).map(OrphanedLine)

  val lineParser = groupParser | settingParser | commentParser | orphanedLine


  val fileParser = lineParser.rep(0, sep = P(eol))


  protected[config] def parseLine(line: String): Option[ParsedLine] = {
    if (line.isEmpty) EmptyLine

    lineParser.parse(line) match {
      case Parsed.Success(value, index) => Some(value)
      case Parsed.Failure(last, index, extra) => None
    }
  }

  def parse(fileContents: String): Config = {
    val lines = Source
      .fromFile(fileContents)
      .getLines()
      .map(parseLine)

    Config.Empty

  }
}

object ConfigLoader
