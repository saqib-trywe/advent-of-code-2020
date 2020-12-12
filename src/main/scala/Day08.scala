package org.trywe.aoc

import scala.io.Source


object Day08 extends App {
  val input = Source.fromFile("src/main/resources/day08.txt")
    .getLines().toSeq.toVector

  println(Program(input).execute())
}

case class Instruction(action: String, arg: Int, var executed: Boolean = false)

class Program(instructions: Vector[Instruction]) {
  private var acc: Int = 0

  def execute(): Int = {
    var idx = 0
    while (!instructions(idx).executed) {
      instructions(idx) match {
        case i@Instruction("acc", arg, _) =>
          acc += arg; i.executed = true; idx += 1
        case i@Instruction("jmp", arg, _) =>
          i.executed = true; idx += arg
        case i =>
          i.executed = true; idx += 1
      }
    }

    acc
  }

  def reset(): Unit = acc = 0

  private val flippedIdx = Int.MinValue
  def flipNextJmpAndNop: Program = {
    val zippedInstructions = instructions.zipWithIndex
    val flippedInstructions = zippedInstructions.takeWhile(_._2 <= flippedIdx)
    ???
  }
}

object Program {
  private val instructionPattern = """^(\p{Alpha}{3}) ([+-]\p{Digit}+)""".r

  def apply(rawInstructions: Vector[String]): Program = {
    val instructions = rawInstructions.map {
      case instructionPattern(instr, arg) => Instruction(instr, arg.toInt)
    }
    new Program(instructions)
  }
}
