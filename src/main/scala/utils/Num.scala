package utils

object Num {
  def toInt(s: String): Int = {
    try {
      s.toInt
    } catch {
      case _: Exception => -1
    }
  }
  def toInt(s: Char): Int = {
    try {
      s.toInt
    } catch {
      case _: Exception => -1
    }
  }
}
