package org.trywe.aoc

import scala.io.Source

object Day09 extends App {
  val  rawInput = Source.fromFile("src/main/resources/day09.txt").getLines().map(_.toLong).toVector

  for(i <- 25 to (rawInput.size - 1)){
    val input = rawInput.slice(i - 25, i)
    val premutations = (for {
      a <- input
      b <- input
      if a != b
    } yield (a, b)).map(x => x._1 + x._2)

    // 15690279
    if(!premutations.contains(rawInput(i)))
      println(s"${rawInput(i)}")
  }
}