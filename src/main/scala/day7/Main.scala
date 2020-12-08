package day7

import utils.Num.toInt

import scala.collection.mutable
import scala.io.Source

object Main extends App {
  val filename = "src/main/scala/day7/input.txt"
  val file = Source.fromFile(filename)
  val lines = file.getLines.toList

  def part1(lines: List[String]): Int = {
    val bags = parse(lines)
    var total = 0

    for (bag <- bags)
      if (bag.name != "shiny gold")
        if (findShinyGold(bag))
          total += 1

    total
  }

  def findShinyGold(bag: Bag): Boolean = {
    if (bag.name == "shiny gold")
      return true

    if (bag.containingBags.length == 0)
      return false

    for (cBag <- bag.containingBags)
      if (findShinyGold(cBag._1))
        return true

    false
  }

  def part2(lines: List[String]): Int = {
    val bags = parse(lines)
    for (bag <- bags) {
      if (bag.name == "shiny gold")
        return countBags(bag, 0)
    }
    -1
  }

  def countBags(bag: Bag, counter: Int): Int = {
    if (bag.containingBags.length == 0)
      return 0

    var total = 0
    for (cBag <- bag.containingBags)
      total += cBag._2 + cBag._2 * countBags(cBag._1, counter)

    total
  }

  def parse(lines: List[String]): Array[Bag] = {
    var remainingLines = lines
    var bags = mutable.Map[String, Bag]()

    while (remainingLines.nonEmpty) {
      for (line <- remainingLines) {
        val bagString = line.split(" bags contain ") // ["light red", "1 bright white bag, 2 muted yellow bags."]
        val bagName = bagString(0)
        val bag = new Bag(bagName)

        if (bagString(1) == "no other bags.") {
          bags = bags += (bagName -> bag)
          remainingLines = remainingLines.filter(_ != line)
        } else {
          val containingBagStrings = bagString(1).split(", ") // ["1 bright white bag", "2 muted yellow bags."]

          for (string <- containingBagStrings) {
            val parts = string.split(" ") // ["1", "bright", "white", "bag"]
            val name = parts(1) + " " + parts(2)

            if (bags.contains(name))
              bag.containingBags :+= (bags(name), toInt(parts(0)))
          }

          if (bag.containingBags.length == containingBagStrings.length) {
            bags = bags += (bagName -> bag)
            remainingLines = remainingLines.filter(_ != line)
          }
        }
      }
    }
    var result = Array[Bag]()
    for ((_, bag) <- bags)
      result = result :+ bag
    result
  }

  println("Part 1: " + part1(lines))
  println("Part 2: " + part2(lines))

  file.close
}
