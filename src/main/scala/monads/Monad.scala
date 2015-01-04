package monads

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A) : F[A]

  def flatMap[A,B](ma: F[A])(f: A => F[B]) : F[B]

  def map[A,B](ma: F[A])(f: A => B) =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: F[A], mb : F[B])(f: (A,B) => C) : F[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))
}

object Monad {
  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma flatMap f
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)
    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma flatMap f
  }
}
