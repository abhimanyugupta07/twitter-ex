package com.twitter.config

import com.twitter.config.adt.{ConfigValue, NumericListValue, StringListValue, StringValue}

case class ConfigKey(key: String, env: Group)

class Config(underlying: Map[Group, Map[String, ConfigValue[_]]]) {

  def get(group: Group): Map[String, ConfigValue[_]] = underlying.getOrElse(group, Map.empty)

  def get(group: String): Map[String, ConfigValue[_]] = underlying.getOrElse(Group(group), Map.empty)
}

object Config {
  val Empty = new Config(Map.empty[Group, Map[String, ConfigValue[_]]])
}

class ConfigSelection(underlying: Map[String, ConfigValue[_]]) {
  def get(key: String): Option[_] = underlying.get(key).map(_.value)

  def getString(key: String): Option[String] = underlying.get(key) flatMap {
    case StringValue(value) => Some(value)
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