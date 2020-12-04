package day1

import scala.io.Source

object Main extends App {
  val filename = "src/main/scala/day1/input.txt"
  val source = Source.fromFile(filename)
  val lines = source.getLines.toArray
  var p1 = 0
  while (p1 < lines.length) {
    var p2 = p1 + 1
    while (p2 < lines.length) {
      var p3 = p2 + 1
      while (p3 < lines.length) {
        val n1 = lines(p1).toInt
        val n2 = lines(p2).toInt
        val n3 = lines(p3).toInt

        if (n1 + n2 + n3 == 2020)
          println(n1*n2*n3)
        p3 += 1
      }
      p2 += 1
    }
    p1 += 1
  }
  source.close
}
