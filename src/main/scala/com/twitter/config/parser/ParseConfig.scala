package com.twitter.config.parser

import com.twitter.config.adt._
import com.twitter.config.{Config, Group}
import scala.io.Source
import fastparse.all._

trait ParsedLine

case class EmptyLine() extends ParsedLine

case class ParsedComment(comment: String) extends ParsedLine

case class ParsedSettingValue[T](setting: SettingValue[T]) extends ParsedLine

case class ParsedGroup(value: Group) extends ParsedLine

case class ParsedOrphanedLine(value: String) extends ParsedLine

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

  val trueParser = P(("true" | "yes").!).map(_ => true)
  val falseParser = P(("false" | "no").!).map(_ => false)

  val booleanParser: P[BooleanConfigValue] = P(trueParser | falseParser).map(BooleanConfigValue)

  val signParser: P[String] = P("-" | "+").!
  val digitParser = P(signParser.? ~ CharIn('0' to '9').rep(1)).!
  val rawLongParser: P[Long] = digitParser.!.map(_.toLong)

  /**
    * Numeric parser for Long should check Long overflow.
    */
  val numParser: P[LongValue] = P(rawLongParser ~ End).map(LongValue)

  val space  = P(CharIn(Seq('\r', '\n', ' ')).?)

  val numberSeq = P(rawLongParser.!.map(_.toLong).rep(min = 2, sep = ","))

  val numberList: P[NumericListValue] = P(numberSeq ~ End) map NumericListValue

  val stringRaw: P[String] = P(CharsWhile(!",;\r\n".contains(_))).!

  val strParser: P[StringValue] = stringRaw map StringValue

  val strSeq: P[Seq[String]] = P(stringRaw).rep(min = 2, sep = ",")

  val stringList: P[StringListValue] = P(strSeq ~ End) map StringListValue

  val valueParser: P[ConfigValue[_]] = P(numberList | stringList | booleanParser | numParser | strParser)

  val settingKeyParser = P(CharsWhile(_ != '<').rep.! ~ overrideParser.?)

  val settingParser: P[ParsedSettingValue[_]] = P(settingKeyParser ~ space ~ "=" ~ space ~ valueParser).map {
    case (key, groupOverride, value) => {
      ParsedSettingValue(SettingValue(key, value, groupOverride))
    }
  }

  val commentParser: P[ParsedComment] = P(";" ~/ CharsWhile(NonLineEnding).rep.! ~/ CharsWhile(Whitespace).rep(1)).map(ParsedComment)

  val orphanedLine: P[ParsedOrphanedLine] = P(CharsWhile(!";\r\n".contains(_)).rep.!).map(ParsedOrphanedLine)

  val lineParser: P[ParsedLine] = groupParser | settingParser | commentParser | orphanedLine


  val fileParser = lineParser.rep(0, sep = P(eol))


  protected[config] def parseLine(line: String): Option[ParsedLine] = {
    if (line.isEmpty) new EmptyLine

    lineParser.parse(line) match {
      case Parsed.Success(value, index) => Some(value)
      case Parsed.Failure(last, index, extra) => None
    }
  }

  def buildConfig(lines: Iterator[Parsed[ParsedLine]]): Config = {
    lines.foldRight(Config.Empty) { case (line, acc) => {
      var currentGroup: Option[Group] = None

      line match {
        case Parsed.Success(parsed, index) => {
          parsed match {
            case x @ EmptyLine() => acc
            case ParsedGroup(group) => {
              currentGroup = Some(group)
              acc
            }

            case ParsedSettingValue(setting) => {
              if (currentGroup.isEmpty) {
                throw new Exception("No group was defined before settings were defined")
              } else {
                acc.add(currentGroup.get, setting)
              }
            }

            case ParsedComment(comment) => {
              Console.println(s"Found comment $comment")
              acc
            }
            case ParsedOrphanedLine(value) => {
              Console.println(s"Found orphaned line $value")
              acc
            }
          }
        }
        case f @ Parsed.Failure(last, index, extra) => {
          throw new Exception(f.msg)
        }
      }
    }
  }
  }

  def loadConfig(fileName: String): Config = {
    val source = getClass.getResourceAsStream("/" + fileName)

    buildConfig {
      Source.fromInputStream(source).getLines().map(lineParser.parse(_))
    }
  }
}

object ConfigLoader
