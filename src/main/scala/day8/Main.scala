package day8

import utils.Num.toInt

import scala.io.Source

object Main extends App {
  val filename = "src/main/scala/day8/input.txt"
  val file = Source.fromFile(filename)
  val lines = file.getLines.toArray

  def part1(lines: Array[String]): Int = getAccumulator(lines)._1

  def part2(lines: Array[String]): Int = {
    for (i <- lines.indices) {
      val newProgram = lines
      val line = lines(i).split(" ")
      val operation = line(0)
      if (operation == "jmp" || operation == "nop") {
        val newOperation = if (operation == "jmp") "nop" else "jmp"
        newProgram(i) = newOperation + " " + line(1)
        val (acc, finished) = getAccumulator(newProgram)
        if (finished)
          return acc
        newProgram(i) = operation + " " + line(1)
      }
    }
    -1
  }

  def getAccumulator(lines: Array[String]): (Int, Boolean) = {
    var i = 0
    var acc = 0
    var visited: Set[Int] = Set()
    while (i < lines.length) {

      if (visited.contains(i))
        return (acc, false)

      visited += i
      val line = lines(i).split(" ")
      val operation = line(0)
      val operator = line(1).take(1)
      val value = toInt(line(1).substring(1))

      operation match {
        case "nop" => i += 1
        case "jmp" => i = if (operator == "+") i + value else i - value
        case "acc" =>
          acc = if (operator == "+") acc + value else acc - value
          i += 1
        case _ => throw new Error()
      }
    }
    (acc, true)
  }

  println("Part 1: " + part1(lines))
  println("Part 2: " + part2(lines))

  file.close
}
