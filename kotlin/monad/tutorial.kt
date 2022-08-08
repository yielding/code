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
    is Result.Success -> {
      value.value
    }
    is Result.Fail -> {
      -1
    }
  }
}