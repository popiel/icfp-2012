package com {

package object wolfskeep {
  val ROBOT = 'R'
  val WALL = '#'
  val ROCK = '*'
  val LAMBDA = '\\'
  val LIFT = 'L'
  val OPEN = 'O'
  val EARTH = '.'
  val EMPTY = ' '
  val DUST = '^'
}

package wolfskeep {

import scala.io._
import scala.collection.mutable.ListBuffer

case class EndGame(score: Int, outcome: String, path: List[Char])

object State {
  def apply(original: State): State = original.copy
  def apply(source: Source): State = {
    var waterLevel: Int = 0
    var waterRate: Int = 0
    var proofTurns: Int = 10

    def eatMeta(lines: Seq[String]): Seq[String] = {
      if (lines(0).startsWith("Water ")) waterLevel = lines(0).substring(6).toInt
      else if (lines(0).startsWith("Flooding ")) waterRate = lines(0).substring(10).toInt
      else if (lines(0).startsWith("Waterproof ")) proofTurns = lines(0).substring(12).toInt
      else if (lines(0).length > 0) return lines
      return eatMeta(lines.tail)
    }

    val lines = eatMeta(source.getLines.toList.reverse)
    val width = lines.map(_.length).max + 2
    val height = lines.size + 2
    val padded = ("#" * width) + lines.map{(s) => String.format("#%-"+(width - 2)+"s#", s)}.mkString("") + ("#" * width)

    def LEFT = -1
    def RIGHT = 1
    def UP = width
    def DOWN = -width

    val mine = padded.toCharArray

    new State(width, height, mine,
              totalLambdas = mine.count(_ == LAMBDA),
              waterLevel   = waterLevel,
              waterRate    = waterRate,  waterCountdown = waterRate,
              proofTurns   = proofTurns, proofCountdown = proofTurns)
  }
}

class State(
  var width: Int,
  var height: Int,
  var mine: Array[Char],
  var rPos: Int = -1,
  var unstable: List[Int] = null,
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
  def copy = new State(width, height, mine.clone, rPos, unstable, score, collected, totalLambdas, waterLevel, waterRate, waterCountdown, proofTurns, proofCountdown, moves)

  if (rPos < 0) rPos = mine.indexOf(ROBOT)
  if (unstable == null) unstable = findUnstable

  var outcome: Option[EndGame] = None

  def LEFT = -1
  def RIGHT = 1
  def UP = width
  def DOWN = -width

  def findUnstable: List[Int] = {
    val builder = new ListBuffer[Int]
    for (pos <- (UP + RIGHT) until (width * height + DOWN + LEFT)) {
      if (mine(pos) == EMPTY &&
          (mine(pos + UP) == ROCK ||
           (mine(pos + UP) == EMPTY && (mine(pos + UP + LEFT) == ROCK || mine(pos + UP + RIGHT) == ROCK))))
        builder += pos
    }
    builder.toList
  }

  def endGame(finalScore: Int, how: String) = { outcome = Some(EndGame(finalScore, how, moves)); outcome }

  def passable(cell: Char) = (cell == EMPTY || cell == EARTH || cell == LAMBDA)

  val dirMap = Map('L' -> LEFT, 'R' -> RIGHT, 'U' -> UP, 'D' -> DOWN, 'W' -> 0)

  def move(cmd: Char): Option[EndGame] = outcome orElse {
    moves = cmd +: moves
    if (cmd == 'A') return endGame(score + 25 * collected, "Abort")
    score -= 1
    val dir = dirMap(cmd)
    if (mine(rPos + dir) == LIFT && collected == totalLambdas) return endGame(score + 50 * collected, "Completed")
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
    return None
  }

  def updateMine: Option[EndGame] = outcome orElse {
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
            return endGame(score, "Robot crushed")
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
      if (proofCountdown < 0) return endGame(score, "Robot drowned")
    } else {
      proofCountdown = proofTurns
    }
    return None
  }

  def run(cmds: String): Option[EndGame] = {
    (None.asInstanceOf[Option[EndGame]] /: (cmds :+ 'A').filter(cmd => "LRUDWA".contains(cmd))) { (base, cmd) =>
      base.orElse(move(cmd)).orElse(updateMine)
    }
  }

  def mineString = {
    mine.grouped(width).map(_.mkString).toList.reverse.mkString("\n")
  }

  override def toString = {
    mineString + "\n" + outcome.getOrElse(EndGame(score, "unfinished", moves))
  }
}

object Lifter {

  @volatile var entry: String = "A"

  def main(args : Array[String]) {
    Runtime.getRuntime.addShutdownHook(new Thread { override def run { println(entry.toString) }})

    val startingState = State(Source.stdin)
    var state = startingState

    for (arg <- args) state.run(arg)
    entry = state.moves.reverse.mkString

    println(state.toString)
  }
}

}}
