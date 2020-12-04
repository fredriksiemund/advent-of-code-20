package day2

import scala.io.{BufferedSource, Source}

object Main extends App {
  def part1(lines: Array[String]): Int = {
    var validPasswords = 0

    for (row <- lines) {
      val a = row.split(": ") // = ["1-7 j", "vrfjljjwbsv"]
      val password = a(1)
      val b = a(0).split(" ") // = ["1-7", "j"]
      val char = b(1).head
      val c = b(0).split("-") // = ["1", "7"]
      val min = c(0).toInt
      val max = c(1).toInt
      val count = password.count(_ == char)
      if (count <= max && count >= min)
        validPasswords += 1
    }
    validPasswords
  }

  def part2(lines: Array[String]): Int = {
    var validPasswords = 0

    for (row <- lines) {
      val a = row.split(": ") // = ["1-7 j", "vrfjljjwbsv"]
      val password = a(1)
      val b = a(0).split(" ") // = ["1-7", "j"]
      val char = b(1).head
      val c = b(0).split("-") // = ["1", "7"]
      val pos1 = c(0).toInt - 1
      val pos2 = c(1).toInt - 1
      val isCorrectAtPos1 = password.charAt(pos1) == char
      val isCorrectAtPos2 = password.charAt(pos2) == char
      if (isCorrectAtPos1 != isCorrectAtPos2)
        validPasswords += 1
    }
    validPasswords
  }

  val filename = "src/main/scala/day2/input.txt"
  val file = Source.fromFile(filename)
  val lines = file.getLines.toArray

  println("Part 1: " + part1(lines))
  println("Part 2: " + part2(lines))

  file.close
}
