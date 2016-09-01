package com.twitter

import com.twitter.config.adt.ConfigValue

package object config {

  implicit def configMapToSelection(configMap: Map[String, ConfigValue[_]]): ConfigSelection = {
    new ConfigSelection(configMap)
  }


}
