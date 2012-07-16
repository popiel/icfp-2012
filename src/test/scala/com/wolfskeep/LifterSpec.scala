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

    it("should make a ramp for lift") {
      val input = """|##*..
                     |L..#.
                     |#R##.""".stripMargin
      val state = State(Source.fromString(input))
      import state.base._
      original(liftPos) should equal (LIFT)
      val ramp = makeRamp(liftPos)
      ramp(liftPos) should equal (0)
      ramp(liftPos + RIGHT) should equal (1)
      ramp(state.rPos) should equal (2)
      ramp(state.rPos + RIGHT + UP + UP) should equal (3)
      ramp(state.rPos + RIGHT + RIGHT + RIGHT) should equal (7)
    }

    it("should read flood maps correctly") {
      val state = State(Source.fromFile("maps/flood1.map"))
      state.waterLevel should equal (1)
      state.base.waterRate should equal (8)
      state.base.proofTurns should equal (5)
    }

    it("should count water turns correctly") {
      val state = State(Source.fromFile("maps/flood1.map"))
      state.run("LLLLDDRRRRLDRDL").waterLevel should equal (2)
      state.run("LLLLDDRRRRLDRDLR").waterLevel should equal (3)
      state.run("LLLLDDRRRRLDRDLR").proofCount should equal (0)
      state.run("LLLLDDRRRRLDRDLRD").proofCount should equal (1)
      state.run("LLLLDDRRRRLDRDLRDL").proofCount should equal (2)
      state.run("LLLLDDRRRRLDRDLRDLR").proofCount should equal (3)
      state.run("LLLLDDRRRRLDRDLRDLRR").proofCount should equal (4)
      state.run("LLLLDDRRRRLDRDLRDLRRU").proofCount should equal (5)
      state.run("LLLLDDRRRRLDRDLRDLRRUL").outcome should equal ("robot drowned")
    }

    it("should grow beards at the expected rate") {
      val input = """|     .
                     |     .
                     |  WL R
                     |     .
                     |     \
                     |
                     |Growth 2""".stripMargin
      val output1 = """|########
                       |#     .#
                       |# WWW .#
                       |# WWL R#
                       |# WWW .#
                       |#     \#
                       |########""".stripMargin
      val output2 = """|########
                       |#WWWWW.#
                       |#WWWWW.#
                       |#WWWLWR#
                       |#WWWWW.#
                       |#WWWWW\#
                       |########""".stripMargin
      val state = State(Source.fromString(input))
      state.base.growthRate should equal (2)
      state.run("WW").mineString should equal (output1)
      state.run("WWW").mineString should equal (output1)
      state.run("WWWW").mineString should equal (output2)
    }

    it("should grow beards on the beard map") {
      val state = State(Source.fromFile("maps/beard1.map")).run("RDLRRDDDLLLDDRUU")
      import state.base._
      growthRate should equal (15)
      state.mine(state.rPos + RIGHT) should equal (BEARD)
      state.run("R").rPos should equal (state.rPos)
    }

    it("should break open horocks") {
      val state = State(Source.fromFile("maps/horock1.map")).run("RRDDR")
      import state.base._
      state.mine(state.rPos + LEFT + DOWN) should equal (LAMBDA)
    }

    it("stepping should not corrupt map") {
      val state = State(Source.fromFile("maps/horock1.map"))
      import state.base._
      val pos = state.rPos + RIGHT + RIGHT + DOWN + DOWN + DOWN
      val s2 = state.run("R")
      val s3 = state.run("RR")
      val s4 = state.run("RRD")
      val s5 = state.run("RRDD")
      val s6 = state.run("RRDDR")
      state.mine(pos) should equal (EMPTY)
      s2.mine(pos) should equal (EMPTY)
      s3.mine(pos) should equal (EMPTY)
      s4.mine(pos) should equal (EMPTY)
      s5.mine(pos) should equal (EMPTY)
      s6.mine(pos) should equal (LAMBDA)
    }

    it("should allow horocks to be pushed") {
      val state = State(Source.fromFile("maps/horock1.map")).run("RRDDRDLDDULLDWR")
      import state.base._
      state.mine(state.rPos + RIGHT) should equal (HOROCK)
    }

    it("should do complex things with horocks") {
      val state = State(Source.fromFile("maps/horock1.map")).run("LDDDDRRRDR")
      import state.base._
      state.mine(state.rPos + LEFT) should equal (ROCK)
      state.mine(state.rPos + LEFT + LEFT + UP + UP) should equal (LAMBDA)
    }

    it("should grow beards on the horocks map") {
      val state = State(Source.fromFile("maps/horock3.map")).run("RRRRRRRRRRRRRLULULULULULULULULLLDLDLDDDUUUUURUUURRRDRDDDA")
      import state.base._
      state.mine(liftPos + UP) should equal (BEARD)
    }

  }
}

