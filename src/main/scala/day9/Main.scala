package day9

import utils.Num.toInt

import scala.io.Source

object Main extends App {
  val filename = "src/main/scala/day9/input.txt"
  val file = Source.fromFile(filename)
  val lines = file.getLines.toArray.map(toInt)
  val p1 = part1(lines, 25)
  val p2 = part2(lines, p1)

  def part1(lines: Array[Int], preambleLength: Int): Int = {
    var preamble = lines.take(preambleLength)
    for (i <- preambleLength until lines.length) {
      val currentVal = lines(i)
      if (pairExists(currentVal, preamble))
        preamble = preamble.takeRight(preambleLength - 1) :+ currentVal
      else
        return currentVal
    }
    -1
  }

  def pairExists(currentVal: Int, preamble: Array[Int]): Boolean = {
    for (j <- preamble.indices)
      for (k <- j until preamble.length)
        if (preamble(j) + preamble(k) == currentVal)
          return true
    false
  }

  def part2(lines: Array[Int], value: Int): Int = {
    for (i <- lines.indices) {
      var end = i + 1
      var values = Array(lines(i), lines(end))
      while (values.sum < value & end < lines.length) {
        end += 1
        values = values :+ lines(end)
      }
      if (values.sum == value)
        return values.max + values.min
    }
    -1
  }
  println("Part 1: " + p1)
  println("Part 2: " + p2)

  file.close
}
