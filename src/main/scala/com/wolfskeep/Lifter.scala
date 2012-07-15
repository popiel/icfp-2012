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
  def maybePassable(cell: Char) = (cell == EMPTY || cell == EARTH || cell == LAMBDA || cell == ROCK || cell == ROBOT)
}

package wolfskeep {

import scala.collection.mutable.ListBuffer
import scala.io._
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

case class EndGame(score: Int, outcome: String, path: List[Char]) {
  def moveString = path.reverse.mkString
  override def toString = "Score: " + score + ": " + outcome + ": " + moveString
}

object State {
  def apply(source: Source): State = {
    var waterLevel: Int = 0
    var waterRate : Int = 0
    var proofTurns: Int = 10
    var growthRate: Int = 25
    var razorCount: Int = 0
    val target = scala.collection.mutable.Map[Char,Int]()

    val metas = Map[Regex, String => Unit] (
      """Water *(\d+)""".r                -> { s: String => waterLevel  = s.toInt },
      """Flooding *(\d+)""".r             -> { s: String => waterRate   = s.toInt },
      """Waterproof *(\d+)""".r           -> { s: String => proofTurns  = s.toInt },
      """Growth *(\d+)""".r               -> { s: String => growthRate  = s.toInt },
      """Razors *(\d+)""".r               -> { s: String => razorCount  = s.toInt },
      """Trampoline A targets *(\d+)""".r -> { s: String => target('A') = s.toInt },
      """Trampoline B targets *(\d+)""".r -> { s: String => target('B') = s.toInt },
      """Trampoline C targets *(\d+)""".r -> { s: String => target('C') = s.toInt },
      """Trampoline D targets *(\d+)""".r -> { s: String => target('D') = s.toInt },
      """Trampoline E targets *(\d+)""".r -> { s: String => target('E') = s.toInt },
      """Trampoline F targets *(\d+)""".r -> { s: String => target('F') = s.toInt },
      """Trampoline G targets *(\d+)""".r -> { s: String => target('G') = s.toInt },
      """Trampoline H targets *(\d+)""".r -> { s: String => target('H') = s.toInt },
      """Trampoline I targets *(\d+)""".r -> { s: String => target('I') = s.toInt }
    )

    def eatMeta(lines: Seq[String]): Seq[String] = {
      if (metas.filter(kv => !kv._1.findFirstMatchIn(lines(0)).map((m: Match) => kv._2(m.group(1))).isEmpty).isEmpty &&
          lines(0).length > 0) lines else eatMeta(lines.tail)
    }

    val lines = eatMeta(source.getLines.toList.reverse)
    val width = lines.map(_.length).max + 2
    val height = lines.size + 2
    val padded = ("#" * width) + lines.map{(s) => String.format("#%-"+(width - 2)+"s#", s)}.mkString("") + ("#" * width)

    val mine = padded.toCharArray
    val state = new BaseState(width, height, mine, waterLevel, waterRate, proofTurns, growthRate,
                              target.mapValues((t:Int) => mine.indexOf(t + '0')).toMap).toState

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
  val proofTurns: Int,
  val growthRate: Int,
  val trampolines: Map[Char, Int]
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

  def toState = new State(this, Map().withDefault(original(_)), original.indexOf(ROBOT), null, waterLevel)

  def makeRamp(pos: Int, ramp: Array[Int] = new Array[Int](width * height)): Array[Int] = {
    def spread(pos: Int) {
      var v = ramp(pos) + 1
      var l = pos + LEFT
      while (maybePassable(original(l)) && ramp(l) > v) {
        ramp(l) = v
        l += LEFT
        v += 1
      }
      v = ramp(pos) + 1
      var r = pos + RIGHT
      while (maybePassable(original(r)) && ramp(r) > v) {
        ramp(r) = v
        r += RIGHT
        v += 1
      }
      v = ramp(pos) + 1
      for (p <- pos until l by LEFT) {
        if (maybePassable(original(p + UP  )) && ramp(p + UP  ) > v) { ramp(p + UP  ) = v; spread(p + UP  ) }
        if (maybePassable(original(p + DOWN)) && ramp(p + DOWN) > v) { ramp(p + DOWN) = v; spread(p + DOWN) }
        v += 1
      }
      v = ramp(pos) + 1
      for (p <- pos until r by RIGHT) {
        if (maybePassable(original(p + UP  )) && ramp(p + UP  ) > v) { ramp(p + UP  ) = v; spread(p + UP  ) }
        if (maybePassable(original(p + DOWN)) && ramp(p + DOWN) > v) { ramp(p + DOWN) = v; spread(p + DOWN) }
        v += 1
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
  val waterCount: Int = 0,
  val proofCount: Int = 0,
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
              waterLevel, waterCount, proofCount,
              moves, score, collected, outcome).withGoals(goals)
  }

  var goals: Seq[Int] = Nil

  def withGoals(g: Seq[Int]) = { goals = g; this }

  def priority = (((score - 25 * collected, rPos) /: (goals.filter(t => mine(t) == LAMBDA || mine(t) == LIFT)))
                  { (sp, t) => (sp._1 - base.ramps(t)(sp._2), t) })._1
  // def priority = score - goals.headOption.map(p => base.ramps(p)(rPos)).getOrElse(0)

  def move(cmd: Char): State = if (outcome != null) this else {
    if (cmd == 'A') {
      val next = new State(base, mine, rPos, unstable,
                           waterLevel, waterCount, proofCount,
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
      val next = new State(base, mine, rPos + dir, unstable,
                           waterLevel, waterCount, proofCount,
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
    val nextProof = if (nextPos <= (waterLevel + 1) * width) proofCount + 1 else 0
    if (nextProof > proofTurns) death = "robot drowned"
    val next = new State(base, nextMine.withDefault(mine(_)), nextPos, future,
                         if (waterCount == waterRate - 1) waterLevel + 1 else waterLevel,
                         if (waterCount == waterRate - 1) 0 else waterCount + 1,
                         nextProof, realCmd +: moves, nextScore, nextCollected, death)
    next.goals = goals.filter(t => next.mine(t) == LAMBDA || next.mine(t) == LIFT)
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
      if (collected == totalLambdas) {
        if (rPos + DOWN  == liftPos) list = 'D' +: list
        if (rPos + RIGHT == liftPos) list = 'R' +: list
        if (rPos + LEFT  == liftPos) list = 'L' +: list
        if (rPos + UP    == liftPos) list = 'U' +: list
      }
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
    queue += startingState.clone.withGoals(lambdas :+ base.liftPos)

    try {
      while (!queue.isEmpty) {
        val state = queue.dequeue()
        val children = state.legalMoves.map(state.move(_))
        val win = children.find(_.rPos == base.liftPos)
        if (!win.isEmpty) return
        queue ++= children
      }
    } catch {
      case d: ThreadDeath => throw d
      case t: Throwable => println(t.toString.toLowerCase); return
    }
  }
}

}}
