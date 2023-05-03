sealed class Result<T> {
  data class Success<T>(val value: T): Result<T>()
  class Fail<T>: Result<T>()
}

fun <T> operator(a: T, op: (T, T) -> T): (T) -> T {
  return { b -> op(a, b) }
}

fun b(value: Result<Int>): Result<Int> {
  return when (value) {
    is Result.Success -> Result.Success(value = value.value * 10)
    is Result.Fail -> value
  }
}

fun main(args: Array<String>) {
  val op10plus = operator(10) { a, b -> a + b}

  val numberList = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val result = numberList.map { op10plus(it) }

  println(result)
}
