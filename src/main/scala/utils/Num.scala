package utils

object Num {
  def toInt(s: String): Int = {
    try {
      s.toInt
    } catch {
      case _: Exception => -1
    }
  }
}
