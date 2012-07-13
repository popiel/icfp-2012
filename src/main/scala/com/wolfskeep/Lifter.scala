package com.wolfskeep

import scala.io._
import scala.collection.mutable.ListBuffer

class State(
  var mine: Array[Char] = null,
  var rPos: Int = 0,
  var unstable: List[Int] = List.empty,
  var score: Int = 0,
  var collected: Int = 0,
  var totalLambdas: Int = 0,
  var waterLevel: Int = 0,
  var waterRate: Int = 0,
  var waterCountdown: Int = 0,
  var proofTurns: Int = 10,
  var proofCountdown: Int = 10,
  var moves: List[Char] = List.empty
) {
  def copy = new State(mine.clone, rPos, unstable, score, collected, totalLambdas, waterLevel, waterRate, waterCountdown, proofTurns, proofCountdown, moves)
}

/**
 * @author T. Alexader Popiel
 */
object Lifter {

  @volatile var entry: String = "A"

  val ROBOT = 'R'
  val WALL = '#'
  val ROCK = '*'
  val LAMBDA = '\\'
  val LIFT = 'L'
  val OPEN = 'O'
  val EARTH = '.'
  val EMPTY = ' '
  val DUST = '^'

  def main(args : Array[String]) {
    Runtime.getRuntime.addShutdownHook(new Thread { override def run { println(entry.toString) }})

    implicit var mainState = new State

    case class EndGame(score: Int, outcome: String, path: List[Char] = mainState.moves)

    def eatMeta(lines: Seq[String])(implicit state: State): Seq[String] = {
      import state._
      if (lines(0).startsWith("Water ")) waterLevel = lines(0).substring(6).toInt
      else if (lines(0).startsWith("Flooding ")) waterRate = lines(0).substring(10).toInt
      else if (lines(0).startsWith("Waterproof ")) proofTurns = lines(0).substring(12).toInt
      else if (lines(0).length > 0) return lines
      return eatMeta(lines.tail)(state)
    }

    val lines = eatMeta(Source.stdin.getLines.toList.reverse)
    val width = lines.map(_.length).max + 2
    val height = lines.size + 2
    val padded = ("#" * width) + lines.map{(s) => String.format("#%-"+(width - 2)+"s#", s)}.mkString("") + ("#" * width)

    val LEFT = -1
    val RIGHT = 1
    val UP = width
    val DOWN = -width

    def findUnstable(implicit state: State): List[Int] = {
      import state._
      val builder = new ListBuffer[Int]
      for (pos <- (UP + RIGHT) until (width * height + DOWN + LEFT)) {
        if (mine(pos) == EMPTY &&
            (mine(pos + UP) == ROCK ||
             (mine(pos + UP) == EMPTY && (mine(pos + UP + LEFT) == ROCK || mine(pos + UP + RIGHT) == ROCK))))
          builder += pos
      }
      builder.toList
    }

    mainState.mine           = padded.toCharArray
    mainState.unstable       = findUnstable
    mainState.waterCountdown = mainState.waterRate
    mainState.proofCountdown = mainState.proofTurns
    mainState.rPos           = mainState.mine.indexOf(ROBOT)
    mainState.totalLambdas   = mainState.mine.count(_ == LAMBDA)

    val startingState        = mainState.copy

    def updateMine(implicit state: State): Option[EndGame] = {
      import state._
      val cleanup = new ListBuffer[Int]
      for (pos <- unstable.sorted) {
        if (mine(pos) == EMPTY) {
          if (mine(pos + UP) == EMPTY) {
            if (mine(pos + LEFT + UP) == ROCK &&
                (mine(pos + LEFT) == DUST || mine(pos + LEFT) == ROCK || mine(pos + LEFT) == LAMBDA)) {
              mine(pos) = ROCK
              mine(pos + LEFT + UP) = DUST
              cleanup += (pos + LEFT + UP)
            }
            if (mine(pos + RIGHT + UP) == ROCK &&
                (mine(pos + RIGHT) == DUST || mine(pos + RIGHT) == ROCK) &&
                (mine(pos + RIGHT + RIGHT) != EMPTY || mine(pos + RIGHT + RIGHT + UP) != EMPTY)) {
              mine(pos) = ROCK
              mine(pos + RIGHT + UP) = DUST
              cleanup += (pos + RIGHT + UP)
            }
          }
          if (mine(pos + UP) == ROCK) {
            mine(pos) = ROCK
            mine(pos + UP) = DUST
            cleanup += (pos + UP)
          }
          if (mine(pos) == ROCK) {
            if (mine(pos + DOWN) == EMPTY) {
              cleanup += (pos + DOWN)
            }
            if (mine(pos + DOWN) == ROBOT) {
              return Some(new EndGame(score, "Robot crushed"))
            }
          }
        }
      }
      unstable = cleanup.toList
      for (pos <- unstable) {
        mine(pos) = EMPTY
      }
      if (waterRate > 0) {
        waterCountdown -= 1
        if (waterCountdown <= 0) { waterLevel += 1 ; waterCountdown = waterRate }
      }
      if (rPos <= (waterLevel) * width) {
        proofCountdown -= 1
        if (proofCountdown < 0) return Some(new EndGame(score, "Robot drowned"))
      } else {
        proofCountdown = proofTurns
      }
      return None
    }

    def passable(cell: Char) = (cell == EMPTY || cell == EARTH || cell == LAMBDA)

    val dirMap = Map('L' -> LEFT, 'R' -> RIGHT, 'U' -> UP, 'D' -> DOWN, 'W' -> 0)

    def move(cmd: Char)(implicit state: State): Option[EndGame] = {
      import state._

      if (cmd == 'A') return Some(new EndGame(score + 25 * collected, "Abort"))
      val dir = dirMap(cmd)
      if (mine(rPos + dir) == LIFT && collected == totalLambdas) return Some(new EndGame(score + 50 * collected, "Completed"))
      if (mine(rPos + dir) == LAMBDA) { collected += 1; score += 25 }
      if (cmd == 'L' || cmd == 'R') {
        if (mine(rPos + dir) == ROCK && mine(rPos + dir + dir) == EMPTY) {
          mine(rPos + dir + dir) = ROCK
          mine(rPos + dir) = EMPTY
          unstable = List(rPos + DOWN + dir + dir + dir,
                          rPos + DOWN + dir + dir,
                          rPos + DOWN + dir) ++ unstable
        }
      }
      if (passable(mine(rPos + dir))) {
        mine(rPos + dir) = ROBOT
        mine(rPos) = EMPTY
        unstable = rPos +: unstable
        rPos += dir
      }
      score -= 1
      return None
    }

  }
}
