package com.twitter.config.adt

import com.twitter.config.Group

trait ConfigValue[T] {
  def value: T
}

case class BooleanConfigValue(
  value: Boolean
) extends ConfigValue[Boolean]


sealed class SeqValue[T](
  val value: Seq[T]
) extends ConfigValue[Seq[T]]

case class StringValue(
  value: String
) extends ConfigValue[String]

case class LongValue(
  value: Long
) extends ConfigValue[Long]

case class StringListValue(
  override val value: Seq[String]
) extends SeqValue[String](value)

case class NumericListValue(
  override val value: Seq[Long]
) extends SeqValue[Long](value)

case class SettingValue[T](
  key: String,
  value: ConfigValue[T],
  group: Option[Group] = None
) {
  def pair: (String, ConfigValue[T]) = key -> value
}
