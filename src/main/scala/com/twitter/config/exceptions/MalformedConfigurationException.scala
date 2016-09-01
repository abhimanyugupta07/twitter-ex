package com.twitter.config.exceptions

import com.twitter.config.adt.SettingValue

case class MalformedConfigurationException(msg: String) extends RuntimeException(msg)
case class MissingGroupDefinition(
  settingValue: SettingValue[_]
) extends RuntimeException(s"A group definition should precede settings, none was found for ${settingValue}")