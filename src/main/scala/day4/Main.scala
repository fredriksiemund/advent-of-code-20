package day4

import scala.io.Source
import scala.util.matching.Regex

object Main extends App {
  def toInt(s: String): Int = {
    try {
      s.toInt
    } catch {
      case e: Exception => -1
    }
  }

  def part1(lines: Array[String]): Int= {
    val requiredFields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    var currentFields = requiredFields
    var fieldCounter = 0

    for (row <- lines) {
      if (row.nonEmpty) {
        val pairs = row.split(" ")
        for (pair <- pairs) {
          val key = pair.split(":")(0)
          currentFields = currentFields.filter(_ != key)
        }
      } else {
        if (currentFields.isEmpty)
          fieldCounter += 1
        currentFields = requiredFields
      }
    }

    if (currentFields.isEmpty)
      fieldCounter += 1

    fieldCounter
  }

  def verifyField(key: String, value : String): Boolean = {
    var valid = false
    key match {
      case "byr" => {
        val year = toInt(value)
        if (year >= 1920 & year <= 2002)
          valid = true
      }
      case "iyr" => {
        val year = toInt(value)
        if (year >= 2010 & year <= 2020)
          valid = true
      }
      case "eyr" => {
        val year = toInt(value)
        if (year >= 2020 & year <= 2030)
          valid = true
      }
      case "hgt" => {
        val unit = value.takeRight(2)
        val height = toInt(value.slice(0, value.length -2))
        if (unit == "cm") {
          if (height >= 150 & height <= 193)
            valid = true
        } else if (unit == "in") {
          if (height >= 59 & height <=75)
            valid = true
        }
      }
      case "hcl" => {
        val pattern = new Regex("#[a-z0-9]{6}")
        valid = pattern.matches(value)
      }
      case "ecl" => {
        val colors = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
        valid = colors.contains(value)
      }
      case "pid" => {
        val pattern = new Regex("[0-9]{9}")
        valid = pattern.matches(value)
      }
      case _ => valid = false
    }
    valid
  }

  def part2(lines: Array[String]): Int= {
    val requiredFields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    var currentFields = requiredFields
    var fieldCounter = 0

    for (row <- lines) {
      if (row.nonEmpty) {
        // Verify key value pairs
        val pairs = row.split(" ")
        for (pair <- pairs) {
          val keyValue = pair.split(":")
          val key = keyValue(0)
          val value = keyValue(1)
          if (verifyField(key, value))
            currentFields = currentFields.filter(_ != key)
        }
      } else {
        // Increment counter if all required fields are included
        if (currentFields.isEmpty)
          fieldCounter += 1
        currentFields = requiredFields
      }
    }

    if (currentFields.isEmpty)
      fieldCounter += 1

    fieldCounter
  }

  val filename = "src/main/scala/day4/input.txt"
  val file = Source.fromFile(filename)
  val lines = file.getLines.toArray

  println("Part 1: " + part1(lines))
  println("Part 2: " + part2(lines))

  file.close
}
