package applicative

import monads.Functor


trait Applicative[F[_]] extends Functor[F] {
  def apply[A,B](fab: F[A => B])(fa: F[A]) : F[B] =
    map2(fab,fa)((f,x) => f(x))

  def unit[A](a: => A) :F[A]


  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C ) : F[C] =
    apply(map(fa)(f.curried))(fb)

}
