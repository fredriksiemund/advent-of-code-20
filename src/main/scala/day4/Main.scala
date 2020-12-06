package day4

import scala.io.Source
import scala.util.matching.Regex

object Main extends App {
  val filename = "src/main/scala/day4/input.txt"
  val file = Source.fromFile(filename)
  val lines = file.getLines.toArray

  def solve(lines: Array[String], validateValue: Boolean): Int = {
    val requiredFields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

    var counter = 0
    var remainingFields = requiredFields

    for (row <- lines) {
      if (row.nonEmpty) {
        val pairs = row.split(" ")

        // Verify key value pairs
        for (pair <- pairs) {
          val keyValue = pair.split(":")
          val key = keyValue(0)
          val value = keyValue(1)

          val valid = if (validateValue) validField(key, value) else true
          if (valid)
            remainingFields = remainingFields.filter(_ != key)
        }
      } else {
        // Increment counter if all required fields are included
        if (remainingFields.isEmpty)
          counter += 1
        remainingFields = requiredFields
      }
    }

    // Check last passport
    if (remainingFields.isEmpty)
      counter += 1

    counter
  }

  def validField(key: String, value: String): Boolean = {
    var valid = false
    key match {
      case "byr" =>
        val year = toInt(value)
        if (year >= 1920 & year <= 2002)
          valid = true
      case "iyr" =>
        val year = toInt(value)
        if (year >= 2010 & year <= 2020)
          valid = true
      case "eyr" =>
        val year = toInt(value)
        if (year >= 2020 & year <= 2030)
          valid = true
      case "hgt" =>
        val unit = value.takeRight(2)
        val height = toInt(value.take(value.length - 2))
        if (unit == "cm") {
          if (height >= 150 & height <= 193)
            valid = true
        } else if (unit == "in") {
          if (height >= 59 & height <= 75)
            valid = true
        }
      case "hcl" =>
        val pattern = new Regex("#[a-z0-9]{6}")
        valid = pattern.matches(value)
      case "ecl" =>
        val colors = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
        valid = colors.contains(value)
      case "pid" =>
        val pattern = new Regex("[0-9]{9}")
        valid = pattern.matches(value)
      case _ => valid = false
    }
    valid
  }

  def toInt(s: String): Int = {
    try {
      s.toInt
    } catch {
      case e: Exception => -1
    }
  }

  println("Part 1: " + solve(lines, validateValue = false))
  println("Part 2: " + solve(lines, validateValue = true))

  file.close
}
