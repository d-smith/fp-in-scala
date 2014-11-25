package parallel

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}


object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean) : Boolean = false
  }

  def unit[A](a: A) : Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C](a: Par[A], b: Par[B])(f:(A,B) => C) : Par[C] =
    (es:ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get,bf.get))
  }

  def fork[A](a: => Par[A]) : Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A) = fork(unit(a))

  def run[A](es: ExecutorService)(a: Par[A]) : Future[A] =
    a(es)

  def asyncF[A,B](f: A => B) : A => Par[B] =
    a => lazyUnit(f(a))
}
