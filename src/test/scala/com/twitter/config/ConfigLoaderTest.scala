package com.twitter.config

import com.twitter.config.parser.ConfigLoader
import org.scalatest.{FlatSpec, Matchers, OptionValues}

class ConfigLoaderTest extends FlatSpec with Matchers with OptionValues {

  val defaultConfig = "example.tconf"

  it should "successfully load the default configuration with no overrides" in {
    val config = ConfigLoader.loadConfig(defaultConfig)

    config.get("ftp").getString("path").value shouldEqual "/tmp/"
    config.get("ftp").getBool("enabled").value shouldEqual false
    config.get("common").getLong("student_size_limit").value shouldEqual 52428800L

    info("It should successfully load a list of params")
    config.get("http").getStringList("params") should contain theSameElementsAs List("array", "of", "values")
  }

  it should "successfully load the default configuration with overrides" in {
    val config = ConfigLoader.loadConfig(defaultConfig, List("ubuntu"))

    info("With the ubuntu override enabled path should have a different value")
    config.get("ftp").getString("path").value shouldEqual "/etc/var/uploads"

    info("It should parse a list of numbers from the configuration")

    config.get("http").getLongList("numlist") should contain theSameElementsAs List(5, 12, 421, 2)
  }
}
