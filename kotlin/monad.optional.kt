import java.io.File

sealed class Optional<T> {
  class None<T>: Optional<T>()
  data class Some<T> (val value: T): Optional<T>()
}

infix fun <T, R> Optional<T>.map(functor: (value: T) -> R): Optional<R> {
  return this.flatMap { value -> Optional.Some(functor(value)) }
}

infix fun <T, R> Optional<T>.flatMap(functor: (value: T) -> Optional<R>): Optional<R> {
  return when (this) {
    is Optional.Some -> functor(this.value)
    is Optional.None -> Optional.None()

  }
}

fun testOptional() {

  fun openFile(fileName: String): Optional<File> {
    return try {
      Optional.Some(File(fileName))
    } catch (e: Throwable) {
      Optional.None()
    }
  }

  fun readFile(file: File): Optional<String> {
    return try {
      Optional.Some(file.inputStream().bufferedReader().use { it.readText() })
    } catch (e: Throwable) {
      Optional.None()
    }
  }

  fun upperCase(value: String): String {
    return value.uppercase()
  }

  val content = openFile("monad.optional.kt") flatMap ::readFile map ::upperCase

  when (content) {
    is Optional.Some -> println(content.value)
    is Optional.None -> println("There is no file.")
  }

}

fun main() {
  testOptional()
}