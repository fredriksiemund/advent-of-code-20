package day15

import utils.Num.toInt

import scala.collection.mutable
import scala.io.Source

object Main extends App {
  val filename = "src/main/scala/day15/input.txt"
  val file = Source.fromFile(filename)
  val lines = file.getLines.toArray
  val input = lines(0).split(",").map(toInt)
  file.close()

  def solve(input: Array[Int], rounds: Int): Int = {
    val log = mutable.Map[Int, Array[Int]]()
    for (i <- input.indices)
      log(input(i)) = Array(i + 1)
    var counter = input.length + 1
    var lastNbr = input.last
    while (counter < rounds + 1) {
      if (log(lastNbr).length > 1) {
        val indexArray = log(lastNbr)
        val nextNbr = indexArray(indexArray.length - 1) - indexArray(indexArray.length - 2)
        log(nextNbr) = get(nextNbr, log) :+ counter
        lastNbr = nextNbr
      } else {
        log(0) = get(0, log) :+ counter
        lastNbr = 0
      }
      counter += 1
    }
    lastNbr
  }

  def get(key: Int, map: mutable.Map[Int, Array[Int]]): Array[Int] = {
    try {
      Array(map(key).last)
    } catch {
      case _: Exception => Array[Int]()
    }
  }

  println("Part 1: " + solve(input, 2020))
  println("Part 2: " + solve(input, 30000000))
}
