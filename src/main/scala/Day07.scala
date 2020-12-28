package org.trywe.aoc

import scala.io.Source

object Day07 extends App {
  val rawInput = Source.fromFile("src/main/resources/day07.txt").getLines().toSeq

  val bagParsePattern = """^\p{Space}*(\p{Digit}+) ([\p{Alpha} ]+)\p{Space}*$""".r
  val input = rawInput.map(_.split(" bags contain ").toSeq)
    .map(s => s.head -> s(1).split(" bags?[,.]").toSeq).toMap
    .mapValues( cbs => cbs.collect { case bagParsePattern(n, b) => (b, n.toInt) } )
    .toMap


  def findBagsThatCanContainBagOfColour(colour: String): Set[String] = {
    def go(theInput: Map[String, Seq[(String, Int)]], foundInBags: Set[String] = Set.empty): Set[String] = {
      var foundInBagsTmp = scala.collection.mutable.Set.empty[String]
      for((bag, bagsIn) <- theInput){
        val found = bagsIn.filter(_._1 == colour)
        if(!found.isEmpty) {
          foundInBagsTmp.add(bag)
        } else {
          val newTheInput = (for {
            b <- bagsIn
          } yield {
              theInput.get(b._1).map(b._1 -> _)
          }).flatten.toMap

          go(newTheInput, foundInBags ++ foundInBagsTmp)
        }
      }
      foundInBags ++ foundInBagsTmp
    }

    go(input.filterNot(e => e._1 == colour))
  }

  val res = findBagsThatCanContainBagOfColour("shiny gold")

  println(res.mkString("\n"))
}
