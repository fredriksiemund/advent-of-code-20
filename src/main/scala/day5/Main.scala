package day5

import scala.collection.mutable.ListBuffer
import scala.io.Source


object Main extends App {
  val filename = "src/main/scala/day5/input.txt"
  val file = Source.fromFile(filename)
  val lines = file.getLines.toArray

  def seatIds(lines: Array[String]): ListBuffer[Int] = {
    var highestSeatId = 0
    val seatIds = new ListBuffer[Int]
    for (row <- lines) {
      var rowNbr = (0, 127)
      var colNbr = (0, 7)
      for (char <- row) char match {
        case 'F' => rowNbr = (rowNbr._1, rowNbr._2 - (rowNbr._2 - rowNbr._1 + 1) / 2)
        case 'B' => rowNbr = (rowNbr._2 - (rowNbr._2 - rowNbr._1) / 2, rowNbr._2)
        case 'L' => colNbr = (colNbr._1, colNbr._2 - (colNbr._2 - colNbr._1 + 1) / 2)
        case 'R' => colNbr = (colNbr._2 - (colNbr._2 - colNbr._1) / 2, colNbr._2)
      }
      val seatId = rowNbr._1 * 8 + colNbr._1
      if (seatId > highestSeatId)
        highestSeatId = seatId
      seatIds.append(seatId)
    }
    seatIds.sorted
  }

  def part1(seatIds: ListBuffer[Int]): Int = seatIds.last

  def part2(seatIds: ListBuffer[Int]): Int = {
    var prevId = seatIds.head - 1
    for (id <- seatIds) {
      if (id - prevId > 1)
        return id - 1
      prevId = id
    }
    -1
  }

  val ids = seatIds(lines)
  println("Part 1: " + part1(ids))
  println("Part 2: " + part2(ids))

  file.close
}
