package com.twitter.config

import com.twitter.config.adt._

case class ConfigKey(key: String, env: Group)

case class Config(underlying: Map[Group, Map[String, ConfigValue[_]]]) {

  def get(group: Group): ConfigSelection = ConfigSelection(underlying.getOrElse(group, Map.empty))

  def get(group: String): ConfigSelection = get(Group(group))

  def contains(group: Group): Boolean = underlying.contains(group)

  def contains(group: String): Boolean = underlying.contains(Group(group))

  def add(group: Group, settingValue: SettingValue[_]): Config = {
    val innerMap = get(group).underlying + settingValue.pair
    Config(underlying + (group -> innerMap))
  }
}

object Config {
  val Empty = new Config(Map.empty[Group, Map[String, ConfigValue[_]]])
}

case class ConfigSelection(underlying: Map[String, ConfigValue[_]]) {
  def get(key: String): Option[_] = underlying.get(key).map(_.value)

  def getString(key: String): Option[String] = underlying.get(key) flatMap {
    case StringValue(value) => Some(value)
    case _ => None
  }

  def getBool(key: String): Option[Boolean] = underlying.get(key) flatMap {
    case BooleanConfigValue(value) => Some(value)
    case _ => None
  }

  def getLong(key: String): Option[Long] = underlying.get(key) flatMap {
    case LongValue(value) => Some(value)
    case _ => None
  }

  def getStringList(key: String): Seq[String] = underlying.get(key).fold(Seq.empty[String]) {
    case StringListValue(list) => list
    case _ => Nil
  }

  def getLongList(key: String): Seq[Long] = underlying.get(key).fold(Seq.empty[Long]) {
    case NumericListValue(list) => list
    case _ => Nil
  }
}