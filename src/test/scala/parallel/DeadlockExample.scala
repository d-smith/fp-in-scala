package parallel

import java.util.concurrent.Executors


object DeadlockExample extends App {
  import Par._
  val a = lazyUnit(42 + 1)
  val S = Executors.newFixedThreadPool(1)
  println(equal(S)(a, fork(a)))
  S.shutdown()
}
