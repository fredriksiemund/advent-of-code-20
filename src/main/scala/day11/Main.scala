package day11

import scala.io.Source

object Main extends App {
  val filename = "src/main/scala/day11/input.txt"
  val file = Source.fromFile(filename)
  val lines = file.getLines.toArray
  var matrix = Array[Array[Char]]()
  for (i <- lines.indices)
    matrix :+= lines(i).toCharArray

  def part1(matrix: Array[Array[Char]]): Int = iterate(matrix, countOnlyAdjacent = true)

  def part2(matrix: Array[Array[Char]]): Int = iterate(matrix, countOnlyAdjacent = false)

  def iterate(matrix: Array[Array[Char]], countOnlyAdjacent: Boolean): Int = {
    var matrixCopy = Array[Array[Char]]()
    for (row <- matrix)
      matrixCopy :+= row.clone

    var prevOccupiedSeats = 0
    var occupiedSeats = getOccupiedSeats(matrixCopy, countOnlyAdjacent)
    while (prevOccupiedSeats != occupiedSeats) {
      prevOccupiedSeats = occupiedSeats
      occupiedSeats = getOccupiedSeats(matrixCopy, countOnlyAdjacent)
    }
    occupiedSeats
  }

  def getOccupiedSeats(matrix: Array[Array[Char]], countOnlyAdjacent: Boolean): Int = {
    var occupiedSeats = 0
    var prevMatrix = Array[Array[Char]]()
    for (row <- matrix)
      prevMatrix :+= row.clone

    for (i <- matrix.indices) {
      for (j <- matrix(i).indices) {
        val newSeat = getNewSeat(prevMatrix, i, j, countOnlyAdjacent)
        if (newSeat == '#')
          occupiedSeats += 1
        matrix(i)(j) = newSeat
      }
    }

    occupiedSeats
  }

  def getNewSeat(matrix: Array[Array[Char]], i: Int, j: Int, countOnlyAdjacent: Boolean): Char = {
    var occupiedSeatsCounter = 0

    for (iOffset <- -1 to 1)
      for (jOffset <- -1 to 1)
        if (!(iOffset == 0 & jOffset == 0)) {
          val seat = if (countOnlyAdjacent)
            getSeat(matrix, i + iOffset, j + jOffset)
          else
            searchForSeat(matrix, i, j, (iOffset, jOffset))
          if (seat == '#')
            occupiedSeatsCounter += 1
        }

    val maxOccupiedSeats = if (countOnlyAdjacent) 4 else 5
    if (matrix(i)(j) == 'L' & occupiedSeatsCounter == 0)
      return '#'
    if (matrix(i)(j) == '#' & occupiedSeatsCounter >= maxOccupiedSeats)
      return 'L'

    matrix(i)(j)
  }

  def searchForSeat(matrix: Array[Array[Char]], i: Int, j: Int, delta: (Int, Int)): Char = {
    var iTemp = i + delta._1
    var jTemp = j + delta._2
    while (iTemp < matrix.length & jTemp < matrix(i).length & jTemp >= 0 & iTemp >= 0) {
      val s = getSeat(matrix, iTemp, jTemp)
      if (s == '#' || s == 'L')
        return s
      jTemp += delta._2
      iTemp += delta._1
    }
    '.'
  }

  def getSeat(matrix: Array[Array[Char]], i: Int, j: Int): Char = {
    try {
      matrix(i)(j)
    } catch {
      case _: Exception => '.'
    }
  }

  println("Part 1: " + part1(matrix))
  println("Part 2: " + part2(matrix))

  file.close
}
