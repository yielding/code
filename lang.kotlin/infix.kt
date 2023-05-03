operator fun Int.times(str: String) = str.repeat(this)
operator fun String.get(range: IntRange) = substring(range)

fun main() {
  infix fun Int.times(str: String) = str.repeat(this)
  println(3 times "Bye ")

  val pair = "Ferrari" to "Katrina"
  println(pair)

  infix fun String.onto(other: String) = Pair(this, other)
  val myPair = "McLaren" onto "Lucas"
  println(myPair)

  val sophia = Person("Sophia")
  val claudia = Person("Claudia")
  sophia likes claudia

  println(2 * "Bye ")

  val str = "Always forgive your enemies; nothing annoys them so much."
  println(str[0..14])
}

class Person(val name: String) {
  val likedPerson = mutableListOf<Person>()
  infix fun likes(other: Person) { 
    likedPerson.add(other) 
  }
}