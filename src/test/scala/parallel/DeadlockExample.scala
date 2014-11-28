package parallel

import java.util.concurrent.Executors

//This deadlocks because there's only a single thread in the pool, and we
//are submitting a callable within a callable, e.g. fork(a)
object DeadlockExample extends App {
  import Par._
  val a = lazyUnit(42 + 1)
  val S = Executors.newFixedThreadPool(1)
  println(equal(S)(a, fork(a)))
  S.shutdown()
}
