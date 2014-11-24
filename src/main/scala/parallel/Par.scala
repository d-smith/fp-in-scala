package parallel

import java.util.concurrent.{TimeUnit, Future, ExecutorService}


object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean) : Boolean = false
  }

  def unit[A](a: A) : Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C](a: Par[A])(f:(A,B) => C) : Par[C] = ???

  def fork[A](a: => Par[A]) : Par[A] = ???

  def lazyUnit[A](a: => A) = fork(unit(a))

  def run[A](a: Par[A]) : A = ???
}
