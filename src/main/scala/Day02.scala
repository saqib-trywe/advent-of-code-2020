package org.trywe.aoc

import scala.io.Source

object Day02 extends App {
  val rawInput = Source.fromFile("src/main/resources/day02.txt").getLines
    .map(i => i.split(" "))
    .map{case Array(r, c, pw) => (r, c.replace(":", ""), pw)}
    .toSeq

  val input = rawInput.map{case (r, c, pw) => (rangeStringToRange(r), c, pw)}
  val validPasswords = input.filter{
    case (r, c, pw) => r.contains(pw.count(_ == c.head))
  }

  println(s"Valid passwords count: ${validPasswords.size}")

  val actualInput = rawInput.map{case (r, c, pw) => (rangeStringToIndexes(r, c), pw)}
  val actuallyValidPasswords = actualInput.filter{ case (isfs, pw) => isfs(pw) }

  println(s"Actually valid passwords count: ${actuallyValidPasswords.size}")

  //Valid passwords count: 556
  //Actually valid passwords count: 605

  private def rangeStringToRange(rangeString: String): List[Int] = {
    val rangeLimits = rangeString.split("-").map(_.toInt)
    Range.inclusive(rangeLimits(0), rangeLimits(1)).toList
  }

  private def rangeStringToIndexes(rangeString: String, char: String): String => Boolean = {
    val rangeLimits = rangeString.split("-").map(_.toInt-1)
      s => {
        val r1 = s(rangeLimits(0)) == char.head
        val r2 = s(rangeLimits(1)) == char.head
        (r1 || r2) && !(r1 && r2)
      }
  }
}
