package com.wolfskeep

/*
ScalaTest also supports the behavior-driven development style, in which you
combine tests with text that specifies the behavior being tested. Here's
an example whose text output when run looks like:

A Map
- should only contain keys and values that were added to it
- should report its size as the number of key/value pairs it contains
*/
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class StateSpec extends Spec with ShouldMatchers {
  describe("A State") {
    it("should be readable from a String") {
      val input = """|R   L
                     |#* *#
                     |#* *#
                     |#####""".stripMargin
      val output = """|#######
                      |#R   L#
                      |##* *##
                      |##* *##
                      |#######
                      |#######""".stripMargin
      State(Source.fromString(input)).mineString should equal (output)
    }

    it("should merge some falling rocks") {
      val input = """|R   L
                     |#* *#
                     |#* *#
                     |#####""".stripMargin
      val output = """|#######
                      |#R   L#
                      |##   ##
                      |##***##
                      |#######
                      |#######""".stripMargin
      val state = State(Source.fromString(input))
      state.updateMine
      state.mineString should equal (output)
    }

  }
}

