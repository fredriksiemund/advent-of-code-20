package day16

import utils.Num.toInt

import scala.collection.mutable
import scala.io.Source

object Main extends App {
  val filename = "src/main/scala/day16/input.txt"
  val file = Source.fromFile(filename)
  var lines = file.getLines.toArray
  file.close()

  val rules = mutable.Map[String, Array[Int]]()
  var myTicket = Array[Int]()
  var otherTickets = Array[Array[Int]]()

  // Get rules
  while (lines(0) != "") {
    val line = lines(0).split(": ") // [departure location, 25-863 or 882-957]
    val ranges = line(1).split(" or ") // [25-863, 882-957]
    var values = Array[Int]()
    for (range <- ranges) {
      val r = range.split("-") // [25, 863]
      values ++= toInt(r(0)).to(toInt(r(1))).toArray
    }
    rules(line(0)) = values
    lines = lines.drop(1)
  }

  // Get my ticket
  lines = lines.drop(2)
  myTicket = lines(0).split(",").map(toInt)

  // Get other tickets
  lines = lines.drop(3)
  for (line <- lines)
    otherTickets :+= line.split(",").map(toInt)

  def part1(tickets: Array[Array[Int]], rules: mutable.Map[String, Array[Int]]): Int = {
    var allowedValues = mutable.Set[Int]()
    for ((k,v) <- rules)
      allowedValues ++= v

    var counter = 0;
    for (ticket <- tickets)
      for (field <- ticket)
        if (!allowedValues.contains(field))
          counter += field

    counter
  }

  def part2(myTicket: Array[Int], tickets: Array[Array[Int]], rules: mutable.Map[String, Array[Int]]): Long = {
    var allowedValues = mutable.Set[Int]()
    for ((k,v) <- rules)
      allowedValues ++= v

    var validTickets = Array[Array[Int]]()
    for (ticket <- tickets) {
      var valid = true
      for (field <- ticket)
        if (!allowedValues.contains(field))
          valid = false
      if (valid)
        validTickets :+= ticket
    }

    val fields = Array.fill(myTicket.length){""}
    val remainingRules = rules.clone()
    while (remainingRules.nonEmpty) {
      for (col <- myTicket.indices) {
        val matchingRules = findMatchingRule(col, validTickets, remainingRules)
        if (matchingRules.length == 1){
          remainingRules -= matchingRules(0)
          fields(col) = matchingRules(0)
        }
      }
    }

    var product: Long = 1
    for (i <- fields.indices)
        if (fields(i).startsWith("departure"))
            product *= myTicket(i)

    product
  }

  def findMatchingRule(col: Int, tickets: Array[Array[Int]], rules: mutable.Map[String, Array[Int]]): Array[String] = {
    val matchingRules = mutable.Map[String, Boolean]()
    for ((k,v) <- rules)
      matchingRules(k) = true

    for (ticket <- tickets)
      for ((k,v) <- rules)
        if (!v.contains(ticket(col)))
          matchingRules(k) = false

    var result = Array[String]()
    for ((k,v) <- matchingRules)
      if (v)
        result :+= k

    result
  }



  println("Part 1: " + part1(otherTickets, rules))
  println("Part 2: " + part2(myTicket, otherTickets, rules))
}
