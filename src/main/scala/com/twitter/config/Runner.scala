package com.twitter.config

import com.twitter.config.parser.ConfigLoader

object Runner extends App {

  val config = ConfigLoader.loadConfig("example.tconf")
}
