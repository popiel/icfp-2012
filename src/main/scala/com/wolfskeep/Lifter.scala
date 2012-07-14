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

  def passable     (cell: Char) = (cell == EMPTY || cell == EARTH || cell == LAMBDA)
  def maybePassable(cell: Char) = (cell == EMPTY || cell == EARTH || cell == LAMBDA || cell == ROCK)
}

package wolfskeep {

import scala.io._
import scala.collection.mutable.ListBuffer

case class EndGame(score: Int, outcome: String, path: List[Char]) {
  def moveString = path.reverse.mkString
  override def toString = "Score: " + score + ": " + outcome + ": " + moveString
}

object State {
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
    new BaseState(width, height, mine, waterLevel, waterRate, proofTurns).toState
  }
}

class BaseState(
  val width: Int,
  val height: Int,
  val original: Array[Char],
  val waterLevel: Int,
  val waterRate: Int,
  val proofTurns: Int
) {
  lazy val totalLambdas = original.count(_ == LAMBDA)

  def LL = width + 1;
  def UR = width * height - width - 1;

  def LEFT = -1
  def RIGHT = 1
  def UP = width
  def DOWN = -width

  val dirMap = Map('L' -> LEFT, 'R' -> RIGHT, 'U' -> UP, 'D' -> DOWN, 'W' -> 0)

  def makeMine: scala.collection.mutable.Map[Int, Char] = new scala.collection.mutable.HashMap[Int,Char] { override def default(k: Int): Char = original(k) }

  def toState = new State(this, Map().withDefault(original(_)), original.indexOf(ROBOT), null, waterLevel, waterRate, proofTurns)
}

class State(
  val base: BaseState,
  val mine: Map[Int, Char],
  val rPos: Int,
  givenUnstable: List[Int] = null,
  val waterLevel: Int,
  val waterCountdown: Int,
  val proofCountdown: Int,
  val moves: List[Char] = Nil,
  val score: Int = 0,
  val collected: Int = 0,
  val outcome: String = null
) {
  import base._
 
  lazy val unstable: List[Int] = if (givenUnstable != null) givenUnstable else findUnstable

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

  def move(cmd: Char): State = if (outcome != null) this else {
    if (cmd == 'A') {
      return new State(base, mine, rPos, unstable,
                       waterLevel, waterCountdown, proofCountdown,
                       'A' +: moves, score + 25 * collected, collected, "abort")
    }
    var nextScore = score - 1
    var nextCollected = collected
    val dir = dirMap(cmd)
    val replaced = mine(rPos + dir)
    if (replaced == LIFT && collected == totalLambdas) {
      return new State(base, mine, rPos, unstable,
                       waterLevel, waterCountdown, proofCountdown,
                       cmd +: moves, score + 50 * collected - 1, collected, "completed")
    }
    if (replaced == LAMBDA) {
      nextCollected += 1
      nextScore += 25
      // if (peak.score < score + 25 * collected) peak = EndGame(score + 25 * collected, "Abort", moves)
    }
    var checklist = unstable
    var nextMine = Map[Int,Char]()
    var nextPos = rPos
    var nextProof = proofCountdown
    val realCmd = {
      if ((cmd == 'L' || cmd == 'R') && replaced == ROCK && mine(rPos + dir + dir) == EMPTY) {
        nextMine += (rPos + dir + dir) -> ROCK
        nextMine += (rPos + dir) -> ROBOT
        nextMine += (rPos) -> EMPTY
        checklist = List(rPos + DOWN + dir + dir + dir,
                         rPos + DOWN + dir + dir,
                         rPos + DOWN + dir,
                         rPos) ++ unstable
        nextPos += dir
        cmd
      } else if (passable(replaced)) {
        nextMine += (rPos + dir) -> ROBOT
        nextMine += (rPos) -> EMPTY
        checklist = rPos +: checklist
        nextPos += dir
        if (nextPos > waterLevel * width) {
          nextProof = proofTurns
        }
        cmd
      } else {
        'W'
      }
    }

    var future: List[Int] = Nil
    var death: String = null
    for (pos <- checklist.sorted) {
      if (mine(pos) == EMPTY) {
        if (mine(pos + UP) == EMPTY) {
          if (mine(pos + LEFT + UP) == ROCK &&
              (mine(pos + LEFT) == ROCK || mine(pos + LEFT) == LAMBDA)) {
            nextMine += (pos) -> ROCK
            nextMine += (pos + LEFT + UP) -> EMPTY
            future = (pos + LEFT + UP) +: future
          }
          if (mine(pos + RIGHT + UP) == ROCK && mine(pos + RIGHT) == ROCK &&
              (mine(pos + RIGHT + RIGHT) != EMPTY || mine(pos + RIGHT + RIGHT + UP) != EMPTY)) {
            nextMine += (pos) -> ROCK
            nextMine += (pos + RIGHT + UP) -> EMPTY
            future = (pos + RIGHT + UP) +: future
          }
        }
        if (mine(pos + UP) == ROCK) {
          nextMine += (pos) -> ROCK
          nextMine += (pos + UP) -> EMPTY
          future = (pos + UP) +: future
        }
        if (mine(pos) == ROCK) {
          if (mine(pos + DOWN) == EMPTY) {
            future = (pos + DOWN) +: future
          }
          if (mine(pos + DOWN) == ROBOT) {
            death = "robot crushed"
          }
        }
      }
    }
    var nextLevel = if (waterRate > 0 && waterCountdown <= 1) waterLevel + 1 else waterLevel
    if (nextPos <= (waterLevel) * width) {
      nextProof -= 1
      if (nextProof < 0) death = "robot drowned"
    }
    return new State(base, nextMine.withDefault(mine), nextPos, future,
                     nextLevel,
                     if (waterCountdown <= 1) waterRate else waterCountdown - 1,
                     nextProof,
                     realCmd +: moves, nextScore, nextCollected, death)
  }

  def run(cmds: String) = {
    (this /: (cmds.filter{cmd => "LRUDWA".contains(cmd)})) { _ move _ }
  }

  def mineString = (0 until (width * height)).grouped(width).map(_.map(mine(_)).mkString).toList.reverse.mkString("\n")

  def result = EndGame(score, (if (outcome == null) "unfinished" else outcome), moves)

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
    if (rPos == pos) return Some(this)
    val moves = legalMoves.sortBy(cmd => ramp(rPos + dirMap(cmd)))
    
    (Option[State](null) /: moves) { (b,c) => b orElse {
      val next = move(c)
      if (next.rPos == pos) Some(next) else next.findPath(pos, ramp)
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
    var best = startingState.result

    if (args.size > 0 && args(0) == "-run") {
      val result = (startingState /: (args.tail :+ "A")) { _ run _ }
      entry = result.result.moveString
      println(result.toString)
      return
    }

    val ramps = new collection.mutable.HashMap[Int, Array[Int]] {
      override def default(pos: Int): Array[Int] = { val ramp = startingState.makeRamp(pos); put(pos, ramp); ramp }
    }

    val lambdas = (startingState.base.LL until startingState.base.UR).filter(pos => startingState.base.original(pos) == LAMBDA)

    def findPath(start: State, finish: Int): Option[State] = {
      if (start.rPos == finish) return Some(start)
      val ramp = ramps(finish);
      implicit val order = new Ordering[State] {
        def compare(x: State, y: State): Int = (ramp(x.rPos) - x.score) - (ramp(y.rPos) - y.score)
      }
      val queue = new scala.collection.mutable.PriorityQueue
      queue += start
      while (!queue.isEmpty) {
        val state = queue.dequeue()
        val children = state.legalMoves.map(state.move(_))
        val win = children.find(_.rPos == finish)
        if (!win.isEmpty) return win
        queue ++= children
      }
      None
    }

    def makeTraversal(lambdas: Seq[Int]) {
      val state = (startingState /: lambdas) { (state, target) =>
        val next = findPath(state, target).getOrElse(state)
        if (best.score < state.move('A').score) {
          best = state.move('A').result
          entry = best.moveString
        }
        next
      }
      val finish = if (state.collected == state.base.totalLambdas) {
        val target = state.base.original.indexOf(LIFT)
        findPath(state, target).getOrElse(state)
      } else state
      if (best.score < finish.move('A').score) {
        best = finish.move('A').result
        entry = best.moveString
      }
      println(finish.toString)
    }

    makeTraversal(lambdas)
  }
}

}}
