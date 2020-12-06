package org.trywe.aoc

import scala.io.Source

object Day05 extends App {
  private val rows = Range(0, 127).inclusive.toSeq
  private val seats = Range(0, 7).inclusive.toSeq
  private val input = Source.fromFile("src/main/resources/day05.txt").getLines().toSeq

  def decodeSeatCode(seatCode: String): (Int, Int, Int) = {
    val (rowr, seatr) = seatCode.foldLeft((rows, seats)) {
      case ((r, s), c) if c == 'F' => r.splitAt(r.length / 2)._1 -> s
      case ((r, s), c) if c == 'B' => r.splitAt(r.length / 2)._2 -> s
      case ((r, s), c) if c == 'L' => r -> s.splitAt(s.length / 2)._1
      case ((r, s), c) if c == 'R' => r -> s.splitAt(s.length / 2)._2
    }
    val (row, seat) = (rowr.head, seatr.head)
    val seatId = row * 8 + seat
    (row, seat, seatId)
  }

  // (108,2,866)
  println(input.map(decodeSeatCode).maxBy(_._3))

  val missingBoardingPass = input.map(decodeSeatCode)
    .map { case (r, s, si) => (r, (s, si)) }
    .groupBy(_._1)
    .filterNot(_._2.length == 8)
    .filter(_._2.length == 7).toSeq
    .flatMap(_._2)
    .map{case (r, (s, _)) => (r,s)}
    .groupMap(_._1)(_._2)
    .map{case (r,ss) => r -> seats.diff(ss).head}
    .foldLeft(1){case (acc, (r,s)) => r*8 + s}

  // 583
  println(missingBoardingPass)
}
