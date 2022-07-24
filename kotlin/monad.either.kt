import java.io.File
import java.io.FileNotFoundException

sealed class Either<out L, out R> {
  data class Left <out L> (val value: L): Either<L, Nothing>()
  data class Right<out R> (val value: R): Either<Nothing, R>()
}

infix fun <L, R, P> Either<L, R>.map(functor: (value: R) -> P): Either<L, P> {
  return this.flatMap { value ->
    Either.Right(functor(value))
  }
}

infix fun <L, R, Q> Either<L, R>.flatMap(functor: (value: R) -> Either<L, Q>): Either<L, Q> {
  return when (this) {
    is Either.Left  -> Either.Left(this.value) 
    is Either.Right -> functor(this.value) 
  }
}

fun testEither(fName: String) {

  fun openFile(fileName: String): Either<Throwable, File> {
    return try {
      val file = File(fileName)
      if (file.exists()) 
        Either.Right(file)
      else 
        Either.Left(FileNotFoundException())
    } catch (e: Throwable) {
      Either.Left(e)
    }
  }

  fun readFile(file: File): Either<Throwable, String> {
    return try {
      Either.Right(file.inputStream().bufferedReader().use { it.readText() })
    } catch (e: Throwable) {
      Either.Left(e)
    }
  }

  val content = openFile(fName) flatMap ::readFile

  when (content) {
    is Either.Left  -> println(content.value)
    is Either.Right -> println(content.value)
  }

}

fun main() {
  testEither("monad.either.kt")
  testEither("not-existing-file")
}