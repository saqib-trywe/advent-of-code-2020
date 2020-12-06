package org.trywe.aoc

import scala.io.Source

object Day04 extends App {

  case class Passport(byr: String, iyr: String, eyr: String, hgt: String, hcl: String, evl: String, pid: String,
                      cid: Option[String] = Some(""))

  object Passport {
    def apply(data: Map[String, String]): Option[Passport] = {
      val r = data.map{
        case ("byr", v) => "byr" -> validateByr(v)
        case ("iyr", v) => "iyr" -> validateIyr(v)
        case ("eyr", v) => "eyr" -> validateEyr(v)
        case ("hgt", v) => "hgt" -> validateHgt(v)
        case ("hcl", v) => "hcl" -> validatehcl(v)
        case ("ecl", v) => "ecl" -> validateEcl(v)
        case ("pid", v) => "pid" -> validatePid(v)
        case("cid", _) => "cid" -> None
      }.toMap

      for{
        byr <- r.get("byr").flatten
        iyr <- r.get("iyr").flatten
        eyr <- r.get("eyr").flatten
        hgt <- r.get("hgt").flatten
        hcl <- r.get("hcl").flatten
        ecl <- r.get("ecl").flatten
        pid <- r.get("pid").flatten
      } yield Passport(byr, iyr, eyr, hgt, hcl, ecl, pid)
    }

    private def validateByr(byr: String): Option[String] = Some(byr)
      .filter(_.length == 4)
      .filter(_.toInt >= 1920)
      .filter(_.toInt <= 2002)

    private def validateIyr(iyr: String): Option[String] = Some(iyr)
      .filter(_.length == 4)
      .filter(_.toInt >= 2010)
      .filter(_.toInt <= 2020)

    private def validateEyr(eyr: String): Option[String] = Some(eyr)
      .filter(_.length == 4)
      .filter(_.toInt >= 2020)
      .filter(_.toInt <= 2030)

    private def validateHgt(hgt: String): Option[String] = Some(hgt)
      .map(h => h.splitAt(h.length - 2))
      .flatMap{case (h, u) => h.toIntOption.map(_ -> u)}
      .flatMap{
        case (h, "cm") => Some(h).filter(_ >= 150).filter(_ <=193 ).map(_ => hgt)
        case (h, "in") => Some(h).filter(_ >= 59).filter(_ <= 76).map(_ => hgt)
        case _ => None
      }

    private val validHclChars = Set('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f')
    private def validatehcl(hcl: String): Option[String] = Some(hcl)
      .filter(_.startsWith("#"))
      .map(_.replace("#", ""))
      .filter(_.forall(c => validHclChars.contains(c)))
      .map(_ => hcl)

    private val validEclStrings = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    private def validateEcl(ecl: String): Option[String] = Some(ecl)
      .filter(c => validEclStrings.contains(c))

    private def validatePid(pid: String): Option[String] = Some(pid)
      .filter(_.length == 9)
      .filter(_.forall(_.isDigit))
  }

  val mandatorKeys = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val input = Source.fromFile("src/main/resources/day04.txt").getLines()
    .map(_.trim)
    .foldLeft((Seq[String](), true)) {
      case (acc, cl) if cl.isEmpty =>
        (acc._1, true)
      case (acc, cl) if acc._1.isEmpty =>
        (acc._1 :+ cl, false)
      case (acc, cl) =>
        if (acc._2) ((acc._1 :+ cl), false)
        else (acc._1.takeWhile(_ != acc._1.last) :+ (acc._1.last + " " + cl), false)
    }._1
    .map(_.split("\\p{Space}")
      .toSeq.map(_.split(":").toSeq))
    .map(_.map { case Seq(a, b) => a -> b })
    .map(_.toMap)

  val countOfValid = input.count(_.keySet.intersect(mandatorKeys) == mandatorKeys)

  // 206
  println(countOfValid)

  // 123
  println(input.flatMap(Passport(_)).length)
}
