package day12

import utils.Num

import scala.io.Source

object Main extends App {
  val filename = "src/main/scala/day12/input.txt"
  val file = Source.fromFile(filename)
  val lines = file.getLines.toArray
  val instructions = lines.map(s => (s.charAt(0), Num.toInt(s.drop(1))))


  def part2(instructions: Array[(Char, Int)]): Int = {
    var coords = (0, 0)
    var waypoint = (10, 1)
    for (instruction <- instructions) {
      val (action, value) = instruction
      action match {
        case 'N' => waypoint = move(waypoint, 0, value)
        case 'S' => waypoint = move(waypoint, 180, value)
        case 'E' => waypoint = move(waypoint, 90, value)
        case 'W' => waypoint = move(waypoint, 270, value)
        case 'L' => waypoint = rotateLeft(waypoint, (value / 90 - 1) % 3)
        case 'R' => waypoint = rotateRight(waypoint, (value / 90 - 1) % 3)
        case 'F' => coords = (waypoint._1 * value + coords._1, waypoint._2 * value + coords._2)
      }
    }
    coords._1.abs + coords._2.abs
  }

  def rotateLeft(waypoint: (Int, Int), steps: Int): (Int, Int) = steps match {
    case 0 => (-1 * waypoint._2, waypoint._1)
    case 1 => (-1 * waypoint._1, -1 * waypoint._2)
    case 2 => (waypoint._2, -1 * waypoint._1)
  }

  def rotateRight(waypoint: (Int, Int), steps: Int): (Int, Int) = steps match {
    case 0 => (waypoint._2, -1 * waypoint._1)
    case 1 => (-1 * waypoint._1, -1 * waypoint._2)
    case 2 => (-1 * waypoint._2, waypoint._1)
  }

  def part1(instructions: Array[(Char, Int)]): Int = {
    var coords = (0, 0)
    var direction = 90
    for (instruction <- instructions) {
      val (action, value) = instruction
      action match {
        case 'N' => coords = move(coords, 0, value)
        case 'S' => coords = move(coords, 180, value)
        case 'E' => coords = move(coords, 90, value)
        case 'W' => coords = move(coords, 270, value)
        case 'L' => direction = (direction - value + 360) % 360
        case 'R' => direction = (direction + value + 360) % 360
        case 'F' => coords = move(coords, direction, value)
      }
    }
    coords._1.abs + coords._2.abs
  }

  def move(coords: (Int, Int), direction: Int, value: Int): (Int, Int) = direction match {
    case 0 => (coords._1, coords._2 + value)
    case 90 => (coords._1 + value, coords._2)
    case 180 => (coords._1, coords._2 - value)
    case 270 => (coords._1 - value, coords._2)
  }

  println("Part 1: " + part1(instructions))
  println("Part 2: " + part2(instructions))

  file.close
}
