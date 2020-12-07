package day7

class Bag(val name: String) {
  var containingBags: Array[(Bag, Int)] = Array[(Bag, Int)]()
}
