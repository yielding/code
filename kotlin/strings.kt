fun main(args: Array<String>) {
  val fruits = arrayOf<String>("Apple", "Mango", "Banana", "Orange", "Apple")
  val dist = fruits.distinct()

  for (item in dist)
    println(item)
}