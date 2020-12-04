package day5

import scala.io.Source


object Main extends App {
  def part1(lines: Array[String]): Int= {
    0
  }

  def part2(lines: Array[String]): Int= {
    0
  }

  val filename = "src/main/scala/day5/input.txt"
  val file = Source.fromFile(filename)
  val lines = file.getLines.toArray

  println("Part 1: " + part1(lines))
  println("Part 2: " + part2(lines))

  file.close
}
