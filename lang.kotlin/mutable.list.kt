fun main(args: Array<String>) {
  val numbers: MutableList<Int> = mutableListOf(1, 2, 3, 4, 5)
  val readOnlyView: List<Int> = numbers
  println("my mutable list--" + numbers)
  numbers.add(6)
  println("my mutable list after addition --" + numbers)
  println(readOnlyView)
}