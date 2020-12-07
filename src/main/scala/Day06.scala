package org.trywe.aoc

import scala.io.Source

object Day06 extends App {
  val inputRaw = Source.fromFile("src/main/resources/day06.txt").getLines.toSeq.map(_.trim)
  val input1 = inputRaw.foldLeft(Seq.empty[String]) {
    case (acc, cl) if cl.isBlank =>
      acc :+ ""
    case (acc, cl) if acc.isEmpty =>
      acc :+ cl
    case (acc, cl) =>
      acc.init :+ (acc.last + cl)
  }
    .map(_.toSet)
    .map(a => a -> a.size)

  // 6768
  println(input1.foldLeft(0) { case (a, c) => a + c._2 })

  val input2 = inputRaw.foldLeft(Seq.empty[String]) {
    case (acc, cl) if cl.isBlank =>
      acc :+ ""
    case (acc, cl) if acc.isEmpty =>
      acc :+ cl
    case (acc, cl) =>
      acc.init :+ (acc.last + cl + ";")
  }.map(_.split(";").toSeq.map(_.toSet))
    .map(_.foldLeft(Set[Char]("abcdefghijklmnopqrstuvwxyz": _*)) {
      case (acc, c) =>
        acc.intersect(c)
    })
  println(input2.foldLeft(0){case (acc, c) => acc + c.size})
}

