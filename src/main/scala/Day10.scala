package org.trywe.aoc

import scala.io.Source

object Day10 extends App {
  val input = Source.fromFile("src/main/resources/day10.txt").getLines().map(_.toInt).toSeq.sorted

  val diffs = input.zip(input.tail).map{case (a,b) ⇒ a-b}.groupBy(identity).mapValues(_.size)

  // 2,450
  println(diffs.mkString(","))
}
