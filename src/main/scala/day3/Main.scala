package day3

import scala.io.Source

object Main extends App {
  def iterate(lines: Array[String], right: Int, down: Int): Long = {
    var row = 0
    var col = 0
    var counter = 0
    while (row < lines.length) {
//      println(lines(row).charAt(col))
      if (lines(row).charAt(col) == '#')
        counter += 1
      col = (col + right) % lines(row).length
      row += down
    }
    counter
  }

  def part1(lines: Array[String]): Long = {
    iterate(lines, 3, 1)
  }

  def part2(lines: Array[String]): Long = {
    val a = iterate(lines, 1, 1)
    val b = iterate(lines, 3, 1)
    val c = iterate(lines, 5, 1)
    val d = iterate(lines, 7, 1)
    val e = iterate(lines, 1, 2)
    a * b * c * d * e
  }

  val filename = "src/main/scala/day3/input.txt"
  val file = Source.fromFile(filename)
  val lines = file.getLines.toArray

  println("Part 1: " + part1(lines))
  println("Part 2: " + part2(lines))
  file.close
}
