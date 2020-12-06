package org.trywe.aoc

import scala.io.Source

object Day03 extends App {
  val input = Source.fromFile("src/main/resources/day03.txt").getLines()
    .map(_.toArray)
    .toArray

  trait MoveResult
  case class Result(treesFound: Int, newX: Int, newY: Int) extends MoveResult
  case object TerminalResult extends MoveResult
  case class Slope(dx: Int, dy: Int)

  object MoveResult {
    def startPosition(moveResult: Result): (Int, Int) = moveResult.newX -> moveResult.newY
  }

  def makeMove(slope: Slope)(x: Int, y: Int): MoveResult = {
    if (input.size-1 == y) {
      TerminalResult
    } else {
      val xx =
        if (x + slope.dx > (input(y).size-1)) (x + slope.dx - input(y).size)
        else x + slope.dx
      val yy = y + slope.dy

      Result(countTree(input(yy)(xx)), xx, yy)
    }
  }

  def travel(slopes: Slope*): Long = {
    def go(slope: Slope)(x: Int = 0, y: Int = 0, found: Long = 0): Long = {
      makeMove(slope)(x, y) match {
        case TerminalResult =>
          found
        case Result(f, nx, ny) =>
          go(slope)(nx, ny, found + f)
      }
    }

    slopes.map(go(_)()).product
  }

  // Trees counted: 178
  println(s"Trees counted: ${travel(Slope(3,1))}")

  println(s"Trees counted: ${travel(Slope(1,1),Slope(3,1),Slope(5,1),Slope(7,1),Slope(1,2))}")



  private def countTree(c: Char): Int = {if (c == '#') 1 else 0 }
}
