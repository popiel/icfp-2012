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
  describe("A PriorityQueue") {
    it("should return the highest-value element") {
      val queue = new scala.collection.mutable.PriorityQueue[Int]()
      queue ++= List(3, 4, 1, 6, 7, 3)
      queue.dequeue should equal (7)
      queue.dequeue should equal (6)
      queue.dequeue should equal (4)
      queue.dequeue should equal (3)
      queue.dequeue should equal (3)
    }
  }

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
      State(Source.fromString(input)).move('W').mineString should equal (output)
    }

    it("should drop rocks on stupid people") {
      val input = """|#*#
                     |#R#
                     |# #
                     |###""".stripMargin
      val output = """|#####
                      |## ##
                      |##*##
                      |##R##
                      |#####
                      |#####""".stripMargin
      val state = State(Source.fromString(input)).move('D')
      state.mineString should equal (output)
      state.outcome should equal ("robot crushed")
    }

    it("should correctly score the best solution for map 1") {
      val state = State(Source.fromFile("maps/contest1.map")).run("LDRDDUULLLDDL")
      state.result.score should equal (212)
      state.result.moveString should equal ("LDRDDUULLLDDL")
    }

    it("should die for stupidity in map 1") {
      val state = State(Source.fromFile("maps/contest1.map")).run("DD")
      state.result.score should equal (-2)
      state.result.outcome should equal ("robot crushed")
    }

    it("should move boulders correctly in map 1") {
      val state = State(Source.fromFile("maps/contest1.map")).run("DLRDD")
      state.result.score should equal (45)
      state.result.outcome should equal ("unfinished")
    }

/*
    it("should correctly undo a long sequence") {
      val state = State(Source.fromFile("maps/contest1.map"))
      val original = state.mineString
      state.run("LDRDDUULLLDDL")
      while (!state.moves.isEmpty) state.undo
      state.mineString should equal (original)
      state.score should equal (0)
      state.collected should equal (0)
    }
*/
  }
}

