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

@RunWith(classOf[JUnitRunner])
class MapSpec extends Spec with ShouldMatchers {

  describe("A Map") {

    it("should only contain keys and values that were added to it") {
      Map("ho" -> 12) should (not contain key ("hi") and not contain value (13))
      Map("hi" -> 13) should (contain key ("hi") and contain value (13))
    }

    it("should report its size as the number of key/value pairs it contains") {
      Map() should have size (0)
      Map("ho" -> 12) should have size (1)
      Map("hi" -> 13, "ho" -> 12) should have size (2)
    }
  }
}

