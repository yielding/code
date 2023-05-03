sealed class Result<T> {
  // unit or pure
  data class Success<T>(val value: T) : Result<T> () 
  class Fail<T> : Result<T> ()

  fun value(onSuccess: (T) -> Unit, onFailed: (Result.Fail<T>) -> Unit) {
    return when (this) {
      is Result.Success -> onSuccess(this.value)
      is Result.Fail -> onFailed(this)
    }
  }
}

// flatMap에 pure (= Reuslt.Success) 적용해서 flatMap을 부르는 것
infix fun <T, R> Result<T>.map(functor: (value: T) -> R) : Result<R> {
  return this.flatMap { value ->
    Result.Success(functor(value)) // 이게 ruby code에서 pure
  }
}

infix fun <T, R> Result<T>.flatMap(functor: (value: T) -> Result<R>) : Result<R> {
  return when (this) {
    is Result.Success -> functor(this.value)
    is Result.Fail -> Result.Fail()
  }
}

fun div(a: Int, b: Int): Result<Int> {
  try {
    return Result.Success(a / b)
  } catch (e: Throwable) {
    return Result.Fail()
  }
}

fun a(value: Int): Result<Int> {
  if (value > 0) {
    return Result.Success(value * 10)
  } else {
    return Result.Fail()
  }
}

fun d(value: Int): Int {
  return value * 10
}

fun value(value: Result<Int>): Int {
  return when (value) {
    is Result.Success -> { value.value }
    is Result.Fail    -> { -1 }
  }
}

fun main() {
  val flow1 = a(10) map ::d flatMap ::a map { "SUCCESS: $it" }
  flow1.value (
    onSuccess = { println(it) },
    onFailed  = { println("failed") }
  )
}
