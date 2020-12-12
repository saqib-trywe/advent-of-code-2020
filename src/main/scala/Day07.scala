package org.trywe.aoc

import scala.io.Source

object Day07 extends App {
  val rawInput = Source.fromFile("src/main/resources/day07.txt").getLines().toSeq

  val bagParsePattern = """^\p{Space}*(\p{Digit}+) ([\p{Alpha} ]+)\p{Space}*$""".r
  type BagContents = Seq[Seq[(String, (String, Int))]]
  val input: BagContents = rawInput.map(_.split(" bags contain ").toSeq)
    .map(s => s.head -> s(1).split(" bags?[,.]").toSeq).toMap
    .map{ case (k, s) => s.collect { case bagParsePattern(n, b) => k -> (b, n.toInt) } }
    .toSeq


  def findBagsThatCanContainBagOfColour(colour: String): Set[String] = {
    ???
  }

  println(input)
}
