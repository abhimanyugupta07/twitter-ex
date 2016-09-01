package com.twitter.config

import com.twitter.config.exceptions.{MalformedConfigurationException, MissingGroupDefinition}
import com.twitter.config.parser.ConfigLoader
import org.scalatest.{FlatSpec, Matchers, OptionValues}

class ConfigLoaderTest extends FlatSpec with Matchers with OptionValues {

  val defaultConfig = "example.tconf"

  "ConfigLoader.loadConfig" should "successfully load the default configuration with no overrides" in {
    val config = ConfigLoader.loadConfig(defaultConfig)

    config.contains("common") shouldEqual true
    config.contains("ftp") shouldEqual true
    config.contains("http") shouldEqual true

    config.get("ftp").getString("path").value shouldEqual "/tmp/"

    config.get("ftp").getString("name").value shouldEqual "hello there, ftp uploading"
    config.get("ftp").getBool("enabled").value shouldEqual false
    config.get("common").getLong("student_size_limit").value shouldEqual 52428800L

    info("It should successfully load a list of params")
    config.get("http").getStringList("params") should contain theSameElementsAs List("array", "of", "values")
  }

  "ConfigLoader.loadConfig" should "successfully load the default configuration with overrides" in {
    val config = ConfigLoader.loadConfig(defaultConfig, List("ubuntu"))

    info("With the ubuntu override enabled path should have a different value")
    config.get("ftp").getString("path").value shouldEqual "/etc/var/uploads"

    info("It should parse a list of numbers from the configuration")

    config.get("http").getLongList("numlist") should contain theSameElementsAs List(5, 12, 421, 2)
  }

  "ConfigLoader.loadConfig" should "successfully load the default configuration with multiple overrides" in {
    val config = ConfigLoader.loadConfig(defaultConfig, List("ubuntu", "production"))

    info("With the ubuntu override enabled path should have a different value")
    config.get("ftp").getString("path").value shouldEqual "/etc/var/uploads"

    info("It should parse a list of numbers from the configuration")

    config.get("http").getLongList("numlist") should contain theSameElementsAs List(5, 12, 421, 2)
  }

  "ConfigLoader.loadConfig" should "fail to load a configuration without a group specified before settings" in {
    intercept[MissingGroupDefinition] {
      ConfigLoader.loadConfig("no_group.tconf", List("ubuntu"))
    }
  }

  "ConfigLoader.loadConfig" should "load a configuration and return None for invalid options" in {
    val config = ConfigLoader.loadConfig(defaultConfig, List("ubuntu", "production"))

    info("With the ubuntu override enabled path should have a different value")
    config.get("ftp").getString("path").value shouldEqual "/etc/var/uploads"


    info("Fetching an option of the wrong type should also return empty")
    config.get("common").getString("student_size_limit") shouldBe empty
    config.get("common").getBool("student_size_limit") shouldBe empty
    config.get("common").getStringList("student_size_limit") shouldBe empty
    config.get("common").getLongList("student_size_limit") shouldBe empty


    info("Fetching invalid keys should always return empty")
    config.get("invalid_does_not_exist").underlying shouldBe empty

    config.get("invalid_does_not_exist").getBool("invalid_on_invalid") shouldBe empty

    config.get("invalid_does_not_exist").getString("invalid_on_invalid") shouldBe empty
    config.get("invalid_does_not_exist").getLong("invalid_on_invalid") shouldBe empty

    config.get("invalid_does_not_exist").getLongList("invalid_on_invalid") shouldBe empty
    config.get("invalid_does_not_exist").getStringList("invalid_on_invalid") shouldBe empty
  }

}
