package org.trywe.aoc

import scala.io.Source

object Day01 extends App {
  val input = Source.fromFile("src/main/resources/day01.txt").getLines().map(_.toInt).toSet
  val sum2020Pair = input.subsets(2).find(_.sum == 2020).get
  val sum2020Triple = input.subsets(3).find(_.sum == 2020).get

  //Pair: HashSet(1703, 317) sums to: 539851
  //Triple: HashSet(1081, 315, 624) sums to: 212481360
  println(s"Pair: ${sum2020Pair} sums to: ${sum2020Pair.product}")
  println(s"Triple: ${sum2020Triple} sums to: ${sum2020Triple.product}")
}