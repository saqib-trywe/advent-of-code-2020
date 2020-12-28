package org.trywe.aoc

import java.io.{File, PrintWriter}
import scala.io.Source

object Day11 extends App {
  private val empty = "L"
  private val occupied = "#"
  private val floor = "."

  val testInput =
    """L.LL.LL.LL
      |LLLLLLL.LL
      |L.L.L..L..
      |LLLL.LL.LL
      |L.LL.LL.LL
      |L.LLLLL.LL
      |..L.L.....
      |LLLLLLLLLL
      |L.LLLLLL.L
      |L.LLLLL.LL""".stripMargin
      val input = Source.fromFile("src/main/resources/day11.txt").getLines().toArray.map(_.toArray.map(c ⇒ s"$c"))
//  val testInputDiffs = testInput.split("\n").map(_.trim.map(c ⇒ s"$c").toArray)
  val (rows, cols) = (input.size, input(0).size)
  val (rIdx, cIdx) = (rows - 1, cols - 1)

  var oldInput = input
  var newInput = Array.empty[Array[String]]
  var first = true
  do {
    if(first) first = false
    else {
      oldInput = newInput
    }

    newInput = (for {
      r ← (0 to rIdx)
      c ← (0 to cIdx)
    } yield change(oldInput, r, c)).grouped(cols).toArray.map(_.toArray)
  } while(areDiffered(oldInput, newInput))

  // 2329
  println(newInput.fold(Array.empty)(_ ++ _).count(_ == occupied))

  private def areDiffered(in1: Array[Array[String]], in2: Array[Array[String]]): Boolean = {
    in1.zip(in2).map { case (a, b) ⇒ a.zip(b).map { case (x, y) => if (x == y) 0 else 1 }.sum }.sum > 0
  }

  private def change(i: Array[Array[String]], r: Int, c: Int): String =
      (r, c) match {
        case (0, 0) ⇒ doChange(i(0)(0), i(0).slice(0, 2) ++ i(1).slice(0, 2))
        case (0, `cIdx`) ⇒ doChange(i(0)(cIdx), i(0).slice(cIdx - 1, cIdx + 1) ++ i(1).slice(cIdx - 1, cIdx + 1))
        case (`rIdx`, 0) ⇒ doChange(i(rIdx)(0), i(rIdx - 1).slice(0, 2) ++ i(rIdx).slice(0, 2))
        case (`rIdx`, `cIdx`) ⇒ doChange(i(rIdx)(cIdx), i(rIdx - 1).slice(cIdx - 1, cIdx + 1) ++ i(rIdx).slice(cIdx - 1, cIdx + 1))
        case (0, c) ⇒ doChange(i(0)(c), i(0).slice(c - 1, c + 2) ++ i(1).slice(c - 1, c + 2))
        case (`rIdx`, c) ⇒ doChange(i(rIdx)(c), i(rIdx - 1).slice(c - 1, c + 2) ++ i(rIdx).slice(c - 1, c + 2))
        case (r, 0) ⇒ doChange(i(r)(0), i.slice(r - 1, r + 2).map(_.slice(0, 2)).fold(Array.empty[String])(_ ++ _))
        case (r, `cIdx`) ⇒ doChange(i(r)(cIdx), i.slice(r - 1, r + 2).map(_.slice(cIdx - 1, cIdx + 1)).fold(Array.empty[String])(_ ++ _))
        case (r, c) ⇒ doChange(i(r)(c), i.slice(r - 1, r + 2).map(_.slice(c - 1, c + 2)).fold(Array.empty[String])(_ ++ _))
      }

  private def doChange(p: String, i: Array[String]): String = {
    p match {
      case `empty` if !i.contains(occupied) ⇒ occupied
      case `occupied` if i.count(_ == occupied) >= 5 ⇒ empty
      case x ⇒ x
    }
  }
}
