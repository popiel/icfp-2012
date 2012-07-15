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
    val state = new BaseState(width, height, mine, waterLevel, waterRate, proofTurns).toState

    Lifter.best = state.result
    state
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
  lazy val liftPos = original.indexOf(LIFT)

  def LL = width + 1;
  def UR = width * height - width - 1;

  def LEFT = -1
  def RIGHT = 1
  def UP = width
  def DOWN = -width

  val dirMap = Map('L' -> LEFT, 'R' -> RIGHT, 'U' -> UP, 'D' -> DOWN, 'W' -> 0)

  def toState = new State(this, Map().withDefault(original(_)), original.indexOf(ROBOT), null, waterLevel, waterRate, proofTurns)

  def makeRamp(pos: Int, ramp: Array[Int] = new Array[Int](width * height)): Array[Int] = {
    def spread(pos: Int) {
      var l = pos + LEFT
      while (maybePassable(original(l)) && ramp(l) > ramp(l - LEFT) + 1) {
        ramp(l) = ramp(l - LEFT) + 1
        l += LEFT
      }
      var r = pos + RIGHT
      while (maybePassable(original(r)) && ramp(r) > ramp(r - RIGHT) + 1) {
        ramp(r) = ramp(r - RIGHT) + 1
        r += RIGHT
      }
      for (p <- pos until l by LEFT) {
        if (maybePassable(original(p + UP  )) && ramp(p + UP  ) > ramp(p) + 1) { ramp(p + UP  ) = ramp(p) + 1; spread(p + UP  ) }
        if (maybePassable(original(p + DOWN)) && ramp(p + DOWN) > ramp(p) + 1) { ramp(p + DOWN) = ramp(p) + 1; spread(p + DOWN) }
      }
      for (p <- pos until r by RIGHT) {
        if (maybePassable(original(p + UP  )) && ramp(p + UP  ) > ramp(p) + 1) { ramp(p + UP  ) = ramp(p) + 1; spread(p + UP  ) }
        if (maybePassable(original(p + DOWN)) && ramp(p + DOWN) > ramp(p) + 1) { ramp(p + DOWN) = ramp(p) + 1; spread(p + DOWN) }
      }
    }
    java.util.Arrays.fill(ramp, Integer.MAX_VALUE)
    ramp(pos) = 0
    spread(pos)
    ramp
  }

  val ramps = new collection.mutable.HashMap[Int, Array[Int]] {
    override def default(pos: Int): Array[Int] = { val ramp = makeRamp(pos); put(pos, ramp); ramp }
  }


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

  override def clone: State = {
    new State(base, mine, rPos, unstable,
              waterLevel, waterCountdown, proofCountdown,
              moves, score, collected, outcome).withGoals(goals)
  }

  var goals: Seq[Int] = Nil

  def withGoals(g: Seq[Int]) = { goals = g; this }

  def priority = (((score, rPos) /: goals) { (sp, t) => (sp._1 - base.ramps(t)(sp._2), t) })._1
  // def priority = score - goals.headOption.map(p => base.ramps(p)(rPos)).getOrElse(0)

  def move(cmd: Char): State = if (outcome != null) this else {
    if (cmd == 'A') {
      val next = new State(base, mine, rPos, unstable,
                           waterLevel, waterCountdown, proofCountdown,
                           'A' +: moves, score + 25 * collected, collected, "abort")
      if (Lifter.best.score < next.score) {
        Lifter.best = next.result
        Lifter.entry = Lifter.best.moveString
        println(next.toString)
      }
      return next
    }
    var nextScore = score - 1
    var nextCollected = collected
    val dir = dirMap(cmd)
    val replaced = mine(rPos + dir)
    if (replaced == LIFT && collected == totalLambdas) {
      val next = new State(base, mine, rPos, unstable,
                           waterLevel, waterCountdown, proofCountdown,
                           cmd +: moves, score + 50 * collected - 1, collected, "completed")
      if (Lifter.best.score < next.score) {
        Lifter.best = next.result
        Lifter.entry = Lifter.best.moveString
        println(next.toString)
      }
      return next
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
                         rPos) ++ checklist
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

    val midMine = (Map() ++ nextMine).withDefault(mine(_))

    var future: List[Int] = Nil
    var death: String = null
    for (pos <- checklist.sorted) {
      if (midMine(pos) == EMPTY) {
        if (midMine(pos + UP) == EMPTY) {
          if (midMine(pos + LEFT + UP) == ROCK &&
              (midMine(pos + LEFT) == ROCK || midMine(pos + LEFT) == LAMBDA)) {
            nextMine += (pos) -> ROCK
            nextMine += (pos + LEFT + UP) -> EMPTY
            future = (pos + LEFT + UP) +: future
          }
          if (midMine(pos + RIGHT + UP) == ROCK && midMine(pos + RIGHT) == ROCK &&
              (midMine(pos + RIGHT + RIGHT) != EMPTY || midMine(pos + RIGHT + RIGHT + UP) != EMPTY)) {
            nextMine += (pos) -> ROCK
            nextMine += (pos + RIGHT + UP) -> EMPTY
            future = (pos + RIGHT + UP) +: future
          }
        }
        if (midMine(pos + UP) == ROCK) {
          nextMine += (pos) -> ROCK
          nextMine += (pos + UP) -> EMPTY
          future = (pos + UP) +: future
        }
        if (nextMine.get(pos) == Some(ROCK)) {
          if (midMine(pos + DOWN) == EMPTY) {
            future = (pos + DOWN) +: future
          }
          if (midMine(pos + DOWN + LEFT) == EMPTY) {
            future = (pos + DOWN + LEFT) +: future
          }
          if (midMine(pos + DOWN + RIGHT) == EMPTY) {
            future = (pos + DOWN + RIGHT) +: future
          }
          if (midMine(pos + DOWN) == ROBOT) {
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
    val next = new State(base, nextMine.withDefault(mine(_)), nextPos, future, nextLevel,
                         if (waterCountdown <= 1) waterRate else waterCountdown - 1,
                         nextProof, realCmd +: moves, nextScore, nextCollected, death)
    next.goals =
      if (Some(nextPos) == goals.headOption)
        if (goals.tail == Nil && nextCollected == totalLambdas)
          List(liftPos)
        else goals.tail
      else goals
    if (Lifter.best.score < nextScore + 25 * collected) {
      Lifter.best = next.result
      Lifter.entry = Lifter.best.moveString
      println(next.move('A').toString)
    }
    next
  }

  def run(cmds: String) = {
    (this /: (cmds.filter{cmd => "LRUDWA".contains(cmd)})) { _ move _ }
  }

  def mineString = (0 until (width * height)).grouped(width).map(_.map(mine(_)).mkString).toList.reverse.mkString("\n")

  def result = EndGame(score, (if (outcome == null) "unfinished" else outcome), moves)

  override def toString = (mineString + "\n" + result).toLowerCase

  def legalMoves = {
    var list: List[Char] = Nil
    if (outcome == null) {
      if (passable(mine(rPos + DOWN))) list = 'D' +: list
      if (mine(rPos + RIGHT) == ROCK && mine(rPos + RIGHT * 2) == EMPTY) list = 'R' +: list
      else if (passable(mine(rPos + RIGHT))) list = 'R' +: list
      if (mine(rPos + LEFT ) == ROCK && mine(rPos + LEFT  * 2) == EMPTY) list = 'L' +: list
      else if (passable(mine(rPos + LEFT))) list = 'L' +: list
      if (passable(mine(rPos + UP))) list = 'U' +: list
    }
    list
  }
}

object Lifter {

  @volatile var entry: String = ""
  var best: EndGame = null

  def main(args : Array[String]) {
    // First, set up our time limits
    Runtime.getRuntime.addShutdownHook(new Thread { override def run { println(entry.toString) }})
    val alarm = new Thread { override def run { Thread.sleep(170000); System.exit(1) } }
    alarm.setDaemon(true)
    alarm.start

    val startingState = State(Source.stdin)
    val base = startingState.base

    if (args.size > 0 && args(0) == "-run") {
      val result = (startingState /: (args.tail :+ "A")) { _ run _ }
      entry = result.result.moveString
      println(result.toString)
      return
    }

    val lambdas = (base.LL until base.UR).filter(pos => base.original(pos) == LAMBDA)

    implicit val order = new Ordering[State] {
      def compare(x: State, y: State): Int = x.priority - y.priority
    }
    val queue = new scala.collection.mutable.PriorityQueue[State]
    queue += startingState.clone.withGoals(lambdas)

    while (!queue.isEmpty) {
      val state = queue.dequeue()
      val children = state.legalMoves.map(state.move(_))
      val win = children.find(_.rPos == base.liftPos)
      if (!win.isEmpty) return
      queue ++= children
    }
  }
}

}}
