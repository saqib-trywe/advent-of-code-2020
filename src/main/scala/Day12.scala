package org.trywe.aoc

import Day12.State.manhattanDistance

import scala.io.Source

object Day12 extends App {
  val testInput =
    """F10
      |N3
      |F7
      |R90
      |F11""".stripMargin.split("\n").map(_.trim)
  val input = Source.fromFile("src/main/resources/day12.txt").getLines().toSeq


  sealed trait Direction {
    val right: Direction
    val left: Direction

    def move(d: Int, state: State): (Int, Int)
    def <*(n: Int): Direction = (1 until degreesToTurns(n)).foldLeft(this){case (acc, _) ⇒ acc.left}
    def *>(n: Int): Direction = (1 until degreesToTurns(n)).foldLeft(this){case (acc, _) ⇒ acc.right}
    def degreesToTurns(n: Int): Int = n / 90
  }

  case object North extends Direction {
    val right = East;
    val left = West

    override def move(d: Int, state: State): (Int, Int) = (state.x, state.y + d)
  }

  case object East extends Direction {
    val right = South;
    val left = North

    override def move(d: Int, state: State): (Int, Int) = (state.x + d, state.y)
  }

  case object South extends Direction {
    val right = West;
    val left = East

    override def move(d: Int, state: State): (Int, Int) = (state.x, state.y - d)
  }

  case object West extends Direction {
    val right = North;
    val left = South

    override def move(d: Int, state: State): (Int, Int) = (state.x - d, state.y)
  }

  case class State(facing: Direction = East, x: Int = 0, y: Int = 0)

  object State {
    def move(state: State)(dd: Direction = state.facing, md: Direction = state.facing, d: Int = 0): State = {
      val (x, y) = md.move(d, state)
      state.copy(facing = dd, x, y)
    }

    def manhattanDistance(state: State): Int = state.x.abs + state.y.abs
  }

  val inputParsePattern = """([NSEWFLR])(\p{Digit}+)""".r
  val finalState = input.foldLeft(State()) {
    case (acc, c) ⇒
      c match {
        case inputParsePattern("N", y) ⇒ State.move(acc)(md = North, d = y.toInt)
        case inputParsePattern("S", y) ⇒ State.move(acc)(md = South, d = y.toInt)
        case inputParsePattern("E", x) ⇒ State.move(acc)(md = East, d = x.toInt)
        case inputParsePattern("W", x) ⇒ State.move(acc)(md = West, d = x.toInt)
        case inputParsePattern("L", x) ⇒ State.move(acc)(acc.facing.left <* x.toInt)
        case inputParsePattern("R", x) ⇒ State.move(acc)(acc.facing.right *> x.toInt)
        case inputParsePattern("F", x) ⇒ State.move(acc)(md = acc.facing, d = x.toInt)
      }
  }

  println(manhattanDistance(finalState))
}
