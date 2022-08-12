import java.io.File
import java.io.FileNotFoundException
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.Semaphore

// ------------------------------------------------------------------
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

// ------------------------------------------------------------------
sealed class Optional<T> {
  class None<T>: Optional<T>()
  data class Some<T> (val value: T): Optional<T>()
}

// 아래 Optional.Some이 pure(f(x))
infix fun <T, R> Optional<T>.map(functor: (value: T) -> R): Optional<R> {
  return this.flatMap { value -> Optional.Some(functor(value)) }
}

infix fun <T, R> Optional<T>.flatMap(functor: (value: T) -> Optional<R>): Optional<R> {
  return when (this) {
    is Optional.Some -> functor(this.value)
    is Optional.None -> Optional.None()
  }
}

// ------------------------------------------------------------------

typealias Callback<Err, T> = (Either<Err, T>) -> Unit

interface Scheduler {
  fun execute(command: () -> Unit)
  fun shutdown()
}

object SchedulerIO: Scheduler {
  private var executorService = Executors.newFixedThreadPool(1)

  override fun execute(command: () -> Unit) {
    if (executorService.isShutdown) {
      executorService = Executors.newFixedThreadPool(1)
    }

    executorService.execute(command)
  }

  override fun shutdown() {
    if (!executorService.isShutdown) {
      executorService.shutdown()
    }
  }
}

// object SchedulerMain: Scheduler {
//   override fun execute(command: () -> Unit) {
//     Thread(command).run()
//   }
// 
//   override fun shutdown() = Unit
// }

// ------------------------------------------------------------------
class Future<Err, V>(private var scheduler: Scheduler = SchedulerIO) {
  private var subscribers: MutableList<Callback<Err, V>> = mutableListOf()
  private var cache: Optional<Either<Err, V>> = Optional.None()
  private var semaphore = Semaphore(1)

  //
  // value는 아래 코드를 보면, callback이다. 
  // ex) f(callback) 무지 헤깔린다.
  //     넘어오는 값은 1 -> 5로 증가하는 숫자 
  //
  private var callback: Callback<Err, V> = { value ->
    semaphore.acquire()
    cache = Optional.Some(value)

    // 모두 소비하고 나온다.
    while (subscribers.size > 0) { 
      val subscriber = subscribers.last()
      subscribers = subscribers.dropLast(1).toMutableList()
      scheduler.execute { 
        subscriber.invoke(value)   // subscriber == callback
      }
    }

    semaphore.release()
  }

  fun create(f: (Callback<Err, V>) -> Unit): Future<Err, V> {
    scheduler.execute { f(callback) }

    return this
  }

  fun subscribe(cb: Callback<Err, V>): Disposable {
    semaphore.acquire()
    when (cache) {
      is Optional.None -> {
        subscribers.add(cb)  // each subscriber is also callback
        semaphore.release()
      }
      is Optional.Some -> {
        semaphore.release()
        val c = (cache as Optional.Some<Either<Err, V>>)
        cb.invoke(c.value)
      }
    }

    return Disposable()
  }

  fun <P> map(functor: (value: V) -> P): Future<Err, P> {
    return this.flatMap { value ->
      Future<Err, P>().create { callback ->
        callback(Either.Right(functor(value)))
      }
    }
  }

  fun <Q> flatMap(functor: (value: V) -> Future<Err, Q>): Future<Err, Q> {
    return Future<Err, Q>().create { callback ->
      this.subscribe { value ->
        when (value) {
          is Either.Left  -> { callback(Either.Left(value = value.value)) }
          is Either.Right -> { functor(value.value).subscribe(callback) }
        }
      }
    }
  }

  inner class Disposable {
    fun dispose() { scheduler.shutdown() }
  }
}

fun count(n: Int): Future<Throwable, Int> {
  return Future<Throwable, Int>().create { callback ->
    Thread.sleep(1000)
    callback.invoke(Either.Right(n + 1))
  }
}

  // fun Future::create(f: (Callback<Err, V>) -> Unit): Future<Err, V> {
  //   scheduler.execute { f(callback) }
  //   return this
  // }

fun testFuture(): Future<Throwable, Int> {
  return Future<Throwable, Int>()
    // create 호출 시, 나머지 코드들이 evaluate되지 않고 곧바로 함수쪽으로 실행 
    // 위 주석된 코드처럼 아래의 코드는 Future.callback에 Right(1)을 넘긴다.
    // 즉 cb == Future.callback
    .create { cb ->                    
      Thread.sleep(1000)
      cb.invoke(Either.Right(1))
    }
    .flatMap(::count)
    .map { it + 1 }
    .flatMap(::count)
    .flatMap(::count)
}

// ------------------------------------------------------------------
fun main() {
  val disposable = testFuture().subscribe {
    when (it) {
      is Either.Left  -> print(it.value)
      is Either.Right -> print(it.value)
    }
  }

  Thread.sleep(5000L)
  disposable.dispose()

  val disposable2 = testFuture().subscribe {
    when (it) {
      is Either.Left -> print(it.value)
      is Either.Right -> print(it.value)
    }
  }

  Thread.sleep(5000L)
  disposable2.dispose()
}
