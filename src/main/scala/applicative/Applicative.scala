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

  def traverse[A,B](as: List[A])(f: A=> F[B]):F[List[B]] =
    as.foldRight(unit(List[B]()))((a,mbs) => map2(f(a),mbs)(_::_))

  def sequence[A](fas:List[F[A]]) : F[List[A]] =
    traverse(fas)(fa => fa)

  def replicateM[A](n:Int, fa:F[A]) : F[List[A]] =
    sequence(List.fill(n)(fa))

}
