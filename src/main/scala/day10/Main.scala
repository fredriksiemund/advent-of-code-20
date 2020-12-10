package day10

import utils.Num.toInt

import scala.collection.mutable
import scala.io.Source

object Main extends App {
  val filename = "src/main/scala/day10/input.txt"
  val file = Source.fromFile(filename)
  val lines = file.getLines.toArray.map(toInt).sorted
  val p1 = part1(0 +: lines :+ lines.last + 3)
  val p2 = part2(0 +: lines)

  def part1(adapters: Array[Int]): Int = {
    var joltOneDiffCounter = 0
    var joltThreeDiffCounter = 0
    for (i <- 1 until adapters.length)
      adapters(i) - adapters(i - 1) match {
        case 1 => joltOneDiffCounter += 1
        case 3 => joltThreeDiffCounter += 1
      }
    joltOneDiffCounter * joltThreeDiffCounter
  }

  def part2(adapters: Array[Int]): Long = {
    var tree = mutable.Map[Int, Array[Int]]()
    var branchCount = mutable.Map[Int, Array[Long]]()

    for (i <- adapters.indices) {
      val node = adapters(i)
      val children = adapters.drop(i + 1).filter(_ - node <= 3)
      tree += (node -> children)
    }

    for (node <- adapters.reverse) {
      val children = tree(node)
      if (children.length == 0) // Last node
        branchCount += (node -> Array(1))
      else
        branchCount += (node -> children.map(child => branchCount(child).sum))
    }

    branchCount(0).sum
  }

  println("Part 1: " + p1)
  println("Part 2: " + p2)

  file.close
}
