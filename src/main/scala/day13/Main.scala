package day13

import utils.Num.toInt

import scala.io.Source

object Main extends App {
  val filename = "src/main/scala/day13/input.txt"
  val file = Source.fromFile(filename)
  val lines = file.getLines.toArray

  val earliestDeparture = toInt(lines(0))
  val departures = lines(1).split(",")

  var ids = Array[Int]()
  var idsAndIndex = Array[(Long, Long)]()

  for (i <- departures.indices)
    if (departures(i) != "x") {
      idsAndIndex = idsAndIndex :+ (toInt(departures(i)), i)
      ids = ids :+ toInt(departures(i))
    }


  def part1(earliestDeparture: Int, ids: Array[Int]): Int = {
    var bus: (Int, Int) = (-1, Int.MaxValue) // ID, waitingTime
    for (id <- ids) {
      val waitingTime = earliestDeparture + id - (earliestDeparture % id) - earliestDeparture
      if (waitingTime < bus._2)
        bus = (id, waitingTime)
    }
    bus._1 * bus._2
  }

  def part2(idsAndIndex: Array[(Long, Long)]): Long = {
    var time: Long = 0
    var multiple: Long = idsAndIndex(0)._1
    for (i <- 1 until idsAndIndex.length) {
      val equations = idsAndIndex.take(i + 1)
      while(!checkTimestamps(time, equations))
        time += multiple
      multiple = equations.map(_._1).product
    }
    time
  }

  def checkTimestamps(timestamp: Long, idsAndIndex: Array[(Long, Long)]): Boolean = {
    for ((id, offset) <- idsAndIndex)
      if((timestamp + offset) % id != 0)
        return false
    true
  }

  println("Part 1: " + part1(earliestDeparture, ids))
  println("Part 2: " + part2(idsAndIndex))

  file.close
}
