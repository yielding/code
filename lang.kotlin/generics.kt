class MutableStack<E>(vararg items: E) {
  private val elements = items.toMutableList()
  
  fun push(element: E) = elements.add(element)
  fun peek(): E = elements.last()
  fun pop(): E = elements.removeAt(elements.size - 1)
  fun isEmpty() = elements.isEmpty()
  fun size() = elements.size

  override fun toString() = "MutableStack(${elements.joinToString()})"
}

fun <E> mutableStackOf(vararg elements: E) = MutableStack(*elements)

open class Dog {
  open fun sayHello() {
    println("wow wow")
  }
}

class Yorkshire: Dog() {
  override fun sayHello() {
    println("wif wif!")
  }
}

open class Tiger(val origin: String) {
  fun sayHello() {
    println("A tiger from $origin saying: grrhhh!")
  }
}

class SiberianTiger: Tiger("Siberia")

open class Lion(val name: String, val origin: String) {
  fun sayHello() {
    println("$name, the lion from $origin says: graoh!")
  }
}

class Asiatic(name: String) : Lion(name = name, origin = "India")

fun main() {
  var s = mutableStackOf(1, 2, 3)
  println(s.size())

  val dog: Dog = Yorkshire()
  dog.sayHello()
}