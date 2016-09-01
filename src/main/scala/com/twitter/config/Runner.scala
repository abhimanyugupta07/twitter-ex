package com.twitter.config

import com.twitter.config.parser.ConfigLoader

object Runner extends App {

  val loader = new ConfigLoader()

  val config = loader.loadConfig("example.tconf")
}
