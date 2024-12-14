object Singleton {
  val name: String = "Singleton Object"

  fun greet() {
    println("Hello, I am $name")
  }
}

fun main() {
  Singleton.greet()
}