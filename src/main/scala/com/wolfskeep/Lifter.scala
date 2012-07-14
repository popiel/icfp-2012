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

  def passable     (cell: Char) = (cell == EMPTY || cell == EARTH || cell == LAMBDA)
  def maybePassable(cell: Char) = (cell == EMPTY || cell == EARTH || cell == LAMBDA || cell == ROCK)
}

package wolfskeep {

import scala.io._
import scala.collection.mutable.ListBuffer

case class Undo(
  cmd: Char,
  replaced: Char,
  wasSpace: List[Int],
  wasRock: List[Int],
  oldUnstable: List[Int],
  waterLevel: Int,
  waterCountdown: Int,
  proofCountdown: Int
)

case class EndGame(score: Int, outcome: String, path: List[Undo]) {
  def moveString = {
    path.map { u: Undo =>
      val c = u.cmd;
      if (c == '<') 'L' else if (c == '>') 'R' else c
    }.reverse.mkString
  }
  override def toString = "Score: " + score + ": " + outcome + ": " + moveString
}

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
  var moves: List[Undo] = List.empty
) {
  def copy = new State(width, height, mine.clone, rPos, unstable, score, collected, totalLambdas, waterLevel, waterRate, waterCountdown, proofTurns, proofCountdown, moves)

  if (rPos < 0) rPos = mine.indexOf(ROBOT)
  if (unstable == null) unstable = findUnstable

  var outcome: Option[EndGame] = None
  var peak: EndGame = EndGame(0, "start", Nil)

  def LEFT = -1
  def RIGHT = 1
  def UP = width
  def DOWN = -width

  def dirs = List(UP, LEFT, RIGHT, DOWN)

  def LL = width + 1;
  def UR = width * height - width - 1;

  def findUnstable: List[Int] = {
    val builder = new ListBuffer[Int]
    for (pos <- LL until UR) {
      if (mine(pos) == EMPTY &&
          (mine(pos + UP) == ROCK ||
           (mine(pos + UP) == EMPTY && (mine(pos + UP + LEFT) == ROCK || mine(pos + UP + RIGHT) == ROCK))))
        builder += pos
    }
    builder.toList
  }

  def endGame(finalScore: Int, how: String) = {
    outcome = Some(EndGame(finalScore, how, moves))
    if (peak.score < finalScore) peak = outcome.get
    outcome
  }

  val dirMap = Map('L' -> LEFT, 'R' -> RIGHT, 'U' -> UP, 'D' -> DOWN, 'W' -> 0, '<' -> LEFT, '>' -> RIGHT)

  def move(cmd: Char): Option[EndGame] = outcome orElse {
    if (cmd == 'A') {
      moves = Undo(cmd, ROBOT, Nil, Nil, unstable, waterLevel, waterCountdown, proofCountdown) +: moves
      return endGame(score + 25 * collected, "Abort")
    }
    score -= 1
    val dir = dirMap(cmd)
    val replaced = mine(rPos + dir)
    if (replaced == LIFT && collected == totalLambdas) {
      rPos += dir
      moves = Undo(cmd, replaced, Nil, Nil, unstable, waterLevel, waterCountdown, proofCountdown) +: moves
      return endGame(score + 50 * collected, "Completed")
    }
    if (replaced == LAMBDA) {
      collected += 1
      score += 25
      if (peak.score < score + 25 * collected) peak = EndGame(score + 25 * collected, "Abort", moves)
    }
    val oldUnstable = unstable
    var wasSpace: List[Int] = Nil
    var wasRock:  List[Int] = Nil
    val realCmd = {
      if ((cmd == 'L' || cmd == 'R') && replaced == ROCK && mine(rPos + dir + dir) == EMPTY) {
        mine(rPos + dir + dir) = ROCK
        mine(rPos + dir) = ROBOT
        mine(rPos) = EMPTY
        unstable = List(rPos + DOWN + dir + dir + dir,
                        rPos + DOWN + dir + dir,
                        rPos + DOWN + dir,
                        rPos) ++ unstable
        rPos += dir
        wasSpace = (rPos + dir) +: wasSpace
        if (cmd == 'L') '<' else '>'
      } else if (passable(replaced)) {
        mine(rPos + dir) = ROBOT
        mine(rPos) = EMPTY
        unstable = rPos +: unstable
        rPos += dir
        if (rPos > waterLevel * width) {
          proofCountdown = proofTurns
        }
        cmd
      } else {
        'W'
      }
    }

    val cleanup = new ListBuffer[Int]
    var crushed = false
    for (pos <- unstable.sorted) {
      if (mine(pos) == EMPTY) {
        if (mine(pos + UP) == EMPTY) {
          if (mine(pos + LEFT + UP) == ROCK &&
              (mine(pos + LEFT) == DUST || mine(pos + LEFT) == ROCK || mine(pos + LEFT) == LAMBDA)) {
            mine(pos) = ROCK
            mine(pos + LEFT + UP) = DUST
            cleanup += (pos + LEFT + UP)
            wasRock = (pos + LEFT + UP) +: wasRock
          }
          if (mine(pos + RIGHT + UP) == ROCK &&
              (mine(pos + RIGHT) == DUST || mine(pos + RIGHT) == ROCK) &&
              (mine(pos + RIGHT + RIGHT) != EMPTY || mine(pos + RIGHT + RIGHT + UP) != EMPTY)) {
            mine(pos) = ROCK
            mine(pos + RIGHT + UP) = DUST
            cleanup += (pos + RIGHT + UP)
            wasRock = (pos + RIGHT + UP) +: wasRock
          }
        }
        if (mine(pos + UP) == ROCK) {
          mine(pos) = ROCK
          mine(pos + UP) = DUST
          cleanup += (pos + UP)
          wasRock = (pos + UP) +: wasRock
        }
        if (mine(pos) == ROCK) {
          wasSpace = pos +: wasSpace
          if (mine(pos + DOWN) == EMPTY) {
            cleanup += (pos + DOWN)
          }
          if (mine(pos + DOWN) == ROBOT) {
            crushed = true
          }
        }
      }
    }
    moves = Undo(realCmd, replaced, wasSpace, wasRock, oldUnstable, waterLevel, waterCountdown, proofCountdown) +: moves
    unstable = cleanup.toList
    for (pos <- unstable) {
      mine(pos) = EMPTY
    }
    if (crushed) return endGame(score, "Robot crushed")
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

  def undo {
    if (moves.isEmpty) return
    val m = moves.head
    moves = moves.tail
    for (pos <- m.wasRock)  mine(pos) = ROCK
    for (pos <- m.wasSpace) mine(pos) = EMPTY
    mine(rPos) = m.replaced
    rPos -= dirMap(m.cmd)
    mine(rPos) = ROBOT
    if (m.replaced == LAMBDA) { collected -= 1; score -= 25 }
    if (m.cmd != 'A') score += 1
    waterLevel = m.waterLevel
    waterCountdown = m.waterCountdown
    proofCountdown = m.proofCountdown
    outcome = None
  }

  def run(cmds: String) {
    for {
      cmd <- (cmds :+ 'A')
      if "LRUDWA".contains(cmd)
    } move(cmd)
  }

  def mineString = mine.grouped(width).map(_.mkString).toList.reverse.mkString("\n")

  def result = outcome.getOrElse(EndGame(score, "unfinished", moves))

  override def toString = (mineString + "\n" + result).toLowerCase

  def makeRamp(pos: Int, ramp: Array[Int] = new Array[Int](width * height)): Array[Int] = {
    def spread(pos: Int) {
      var l = pos + LEFT
      while (maybePassable(mine(l)) && ramp(l) > ramp(l - LEFT) + 1) {
        ramp(l) = ramp(l - LEFT) + 1
        l += LEFT
      }
      var r = pos + RIGHT
      while (maybePassable(mine(r)) && ramp(r) > ramp(r - RIGHT) + 1) {
        ramp(r) = ramp(r - RIGHT) + 1
        r += RIGHT
      }
      for (p <- pos until l by LEFT) {
        if (maybePassable(mine(p + UP  )) && ramp(p + UP  ) > ramp(p) + 1) { ramp(p + UP  ) = ramp(p) + 1; spread(p + UP  ) }
        if (maybePassable(mine(p + DOWN)) && ramp(p + DOWN) > ramp(p) + 1) { ramp(p + DOWN) = ramp(p) + 1; spread(p + DOWN) }
      }
      for (p <- pos until r by RIGHT) {
        if (maybePassable(mine(p + UP  )) && ramp(p + UP  ) > ramp(p) + 1) { ramp(p + UP  ) = ramp(p) + 1; spread(p + UP  ) }
        if (maybePassable(mine(p + DOWN)) && ramp(p + DOWN) > ramp(p) + 1) { ramp(p + DOWN) = ramp(p) + 1; spread(p + DOWN) }
      }
    }
    java.util.Arrays.fill(ramp, Integer.MAX_VALUE)
    ramp(pos) = 0
    spread(pos)
    ramp
  }

  def legalMoves = {
    var list: List[Char] = Nil
    if (passable(mine(rPos + DOWN))) list = 'D' +: list
    if (mine(rPos + RIGHT) == ROCK && mine(rPos + RIGHT * 2) == EMPTY) list = 'R' +: list
    else if (passable(mine(rPos + RIGHT))) list = 'R' +: list
    if (mine(rPos + LEFT ) == ROCK && mine(rPos + LEFT  * 2) == EMPTY) list = 'L' +: list
    else if (passable(mine(rPos + LEFT))) list = 'L' +: list
    if (passable(mine(rPos + UP))) list = 'U' +: list
    list
  }

  def findPath(pos: Int, ramp: Array[Int]): Option[State] = {
    val moves = legalMoves.sortBy(cmd => ramp(rPos + dirMap(cmd)))
    (None.asInstanceOf[Option[State]] /: moves) { (base, cmd) => base orElse {
      move(cmd)
      if (rPos == pos) Some(this)
      else findPath(pos, ramp) orElse { undo; None }
    } }
  }
}

object Lifter {

  @volatile var entry: String = ""

  def main(args : Array[String]) {
    // First, set up our time limits
    Runtime.getRuntime.addShutdownHook(new Thread { override def run { println(entry.toString) }})
    val alarm = new Thread { override def run { Thread.sleep(170000); System.exit(1) } }
    alarm.setDaemon(true)
    alarm.start

    val startingState = State(Source.stdin)
    var state = startingState.copy
    var best = state.result

    if (args.size > 0 && args(0) == "-run") {
      for (arg <- args.tail) state.run(arg)
      entry = state.result.moveString
      println(state.toString)
      return
    }

    val ramps = new collection.mutable.HashMap[Int, Array[Int]] {
      override def default(pos: Int): Array[Int] = { val ramp = state.makeRamp(pos); put(pos, ramp); ramp }
    }

    val lambdas = (state.LL until state.UR).filter(pos => state.mine(pos) == LAMBDA)
    // val ramps = lambdas.zip(lambdas.map(pos => state.makeRamp(pos))).toMap

    def makeTraversal(lambdas: Seq[Int]) {
      state = startingState.copy
      for (target <- lambdas) {
        state.findPath(target, ramps(target))
        if (best.score < state.peak.score) {
          best = state.peak
          entry = best.moveString
        }
      }
      if (state.collected == state.totalLambdas) {
        val target = state.mine.indexOf(LIFT)
        state.findPath(target, ramps(target))
      }
      if (best.score < state.peak.score) {
        best = state.peak
        entry = best.moveString
      }
    }

    makeTraversal(lambdas)
    if (best.score < state.result.score) {
      best = state.result
      entry = best.moveString
    }

    println(state.toString)
  }
}

}}
