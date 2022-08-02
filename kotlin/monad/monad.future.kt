import java.io.File
import java.io.FileNotFoundException
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.Future

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

fun run() {
  val executor = Executors.newFixedThreadPool(2)
  val integers = listOf(1, 2, 3, 4, 5)
  val future = executor.submit<int> {
    TimeUnit.MILLISECONDS.sleep(500)
    integers.sum()
  }

  executor.shutdown()
  try {
    val result = future.get()
    print("result: $result")
  } catch (e: InterruptedException) {
    e.printStackTrace()
  } catch (e: ExecutionException) {
    e.printStackTrace()
  }
}

fun main() {
  run()
  //testEither("monad.either.kt")
  //testEither("not-existing-file")
}