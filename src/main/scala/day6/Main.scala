package day6

import scala.io.Source
import scala.collection.mutable.Map

object Main extends App {
  val filename = "src/main/scala/day6/input.txt"
  val file = Source.fromFile(filename)
  val lines = file.getLines.toArray

  def part1(lines: Array[String]): Int = {
    var counter = 0
    var answers: Set[Char] = Set()
    for(line <- lines) {
      if (line.isEmpty) {
        counter += answers.size
        answers = Set()
      } else {
       for (char <- line)
         answers += char
      }
    }
    counter += answers.size
    counter
  }

  def part2(lines: Array[String]): Int = {
    var totalCounter = 0
    var groupCounter = 0
    var answers = scala.collection.mutable.Map[Char, Int]()

    for(line <- lines) {
      if (line.isEmpty) {
        // Check number of answers
        for ((k,v) <- answers)
          if (v == groupCounter)
            totalCounter += 1
        // Reset counters
        groupCounter = 0
        answers = scala.collection.mutable.Map[Char, Int]()
      } else {
        // Increment counters
        groupCounter += 1
        for (char <- line)
          answers.updateWith(char)({
            case Some(count) => Some(count + 1)
            case None        => Some(1)
          })
      }
    }
    // Check answers of last group
    for ((k,v) <- answers)
      if (v == groupCounter)
        totalCounter += 1

    totalCounter
  }

  println("Part 1: " + part1(lines))
  println("Part 2: " + part2(lines))

  file.close
}
