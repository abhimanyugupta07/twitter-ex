package com.twitter.config.parser

import com.twitter.config.exceptions.MissingGroupDefinition
import com.twitter.config.{Config, Group, SettingOverride}
import fastparse.all._

import scala.io.Source
import org.slf4j.LoggerFactory

class ConfigLoader extends ConfigParser {

  val logger = LoggerFactory.getLogger(this.getClass)

  def buildConfig(lines: Iterator[Parsed[ParsedLine]], overrides: List[SettingOverride]): Config = {
    var currentGroup: Option[Group] = None

    lines.foldLeft(Config.Empty) { case (acc, line) => {

      line match {
        case Parsed.Success(parsed, index) => {
          parsed match {
            case x @ EmptyLine() => {
              logger.debug(s"Found an empty line $parsed")
              acc
            }

            case ParsedGroup(group) => {
              logger.debug(s"Found a group called $group")
              currentGroup = Some(group)
              acc
            }

            case ParsedSettingValue(setting) => {
              if (currentGroup.isEmpty) {
                val ex = MissingGroupDefinition(setting)
                logger.error("No group definition found", ex)
                throw ex
              } else {
                setting.envOverride match {
                  case Some(settingOverride) if overrides.contains(settingOverride) => acc.add(currentGroup.get, setting)
                  case Some(settingOverride) => acc
                  case None => acc.add(currentGroup.get, setting)
                }
              }
            }

            case ParsedComment(comment) => {
              logger.debug(s"Found comment $comment")
              acc
            }
            case ParsedOrphanedLine(value) => {
              logger.warn(s"Found orphaned line $value")
              acc
            }
          }
        }
        case f @ Parsed.Failure(last, index, extra) => {
          val ex = new Exception(f.msg)
          logger.error(s"Invalid configuration file at index $index, extra: $extra", ex)
          throw ex
        }
      }
    }
    }
  }

  def loadConfig(fileName: String, overrides: List[SettingOverride] = Nil): Config = {
    val source = getClass.getResourceAsStream("/" + fileName)

    val conf = buildConfig(
      Source.fromInputStream(source).getLines().map(lineParser.parse(_)),
      overrides
    )
    conf
  }
}

object ConfigLoader {

  def loadConfig(file: String, overrides: List[String] = Nil): Config = {
    new ConfigLoader().loadConfig(file, overrides.map(SettingOverride))
  }
}
