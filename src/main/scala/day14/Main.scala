package day14

import utils.Num.toInt

import java.math.BigInteger
import scala.collection.mutable
import scala.io.Source
import scala.math.pow

object Main extends App {
  val filename = "src/main/scala/day14/input.txt"
  val file = Source.fromFile(filename)
  val lines = file.getLines.toArray

  def part1(lines: Array[String]): Long = {
    var mask = mutable.Map[Int, Char]() // (index -> val)
    val result = mutable.Map[Int, Long]()
    for (line <- lines) {
      if (line.startsWith("mask")) {
        mask = getMask(line)
      } else {
        val operation = getOperation(line)
        var binary = operation._2.toBinaryString.reverse.toCharArray
        binary ++= Array.fill(36 - binary.length)('0')
        for (i <- binary.indices)
          if (mask.contains(i))
            binary(i) = mask(i)

        result(operation._1) = new BigInteger(binary.mkString.reverse, 2).longValue()
      }
    }
    var sum: Long = 0
    for ((k,v) <- result)
      sum += v
    sum
  }

  def part2(lines: Array[String]): Long = {
    var mask = mutable.Map[Int, Char]() // (index -> val)
    val result = mutable.Map[Long, Long]()
    for (line <- lines) {
      if (line.startsWith("mask")) {
        mask = getMask(line, includeX = true)
      } else {
        val operation = getOperation(line)
        var binary = operation._1.toBinaryString.reverse.toCharArray
        binary ++= Array.fill(36 - binary.length)('0')
        var floatingIndices = Array[Int]()

        for (i <- binary.indices) {
          if(mask(i) != '0') {
            binary(i) = mask(i)
            if (mask(i) == 'X')
              floatingIndices :+= i
          }
        }

        for (j <- 0 until pow(2, floatingIndices.length).intValue()) {
          val fill = j.toBinaryString.reverse.toCharArray
          val alt = binary.clone()
          for (k <- floatingIndices.indices) {
            val l = floatingIndices(k)
            alt(l) = getAtIndex(fill, k)
          }
          val memLoc = new BigInteger(alt.mkString.reverse, 2).longValue()
          result(memLoc) = operation._2
        }
      }
    }
    var sum: Long = 0
    for ((k,v) <- result)
      sum += v
    sum
  }

  def getAtIndex(array: Array[Char], index: Int): Char = {
    try {
      array(index)
    } catch {
      case _: Exception => '0'
    }
  }

  def getMask(string: String, includeX: Boolean = false): mutable.Map[Int, Char] = {
    var mask = mutable.Map[Int, Char]()
    val maskRaw = string.split(" = ")(1).reverse
    for (i <- maskRaw.toCharArray.indices) {
      if (includeX)
        mask += (i -> maskRaw(i))
      else if (maskRaw(i) != 'X')
        mask += (i -> maskRaw(i))
    }
    mask
  }

  def getOperation(string: String): (Int, Int) = {
    val a = string.split(" = ") // [mem[8], 11]
    val memLocation = a(0).split('[')(1).split(']')(0)
    (toInt(memLocation), toInt(a(1)))
  }

  println("Part 1: " + part1(lines))
  println("Part 2: " + part2(lines))

}
