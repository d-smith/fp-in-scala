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

  def map3[A,B,C,D](fa: F[A],
                    fb: F[B],
                    fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f:(A,B,C,D) => E) : F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

}

sealed trait Validation[+E,+A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E,Nothing]
case class Success[A](a: A) extends Validation[Nothing,A]


object Applicative {
  val streamApplicative = new Applicative[Stream] {
    override def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    override def map2[A,B,C](a: Stream[A], b: Stream[B])(f: (A,B) => C) =
      (a.zip(b)).map(f.tupled)

  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] =
    new Applicative[({type f[x] = Validation[E,x]})#f] {
      def unit[A](a: => A) = Success(a)
      override def map2[A,B,C](fa: Validation[E,A], fb: Validation[E,B])(f:(A,B) => C) =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a,b))
          case (Failure(h1,t1),Failure(h2,t2)) => Failure(h1, t1 ++ Vector(h1) ++ t2)
          case (f@Failure(_,_), _) => f
          case (_, f@Failure(_,_)) => f
        }
    }
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](fa:F[A])(f: A => F[B]) : F[B] = join(map(fa)(f))

  def join[A](ffa: F[F[A]]) : F[A] = flatMap(ffa)(fa => fa)

  def compose[A,B,C](f: A => F[B], g: B => F[C]) : A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A,B](fa: F[A])(f: A => B) : F[B] =
    flatMap(fa)((a:A) => unit(f(a)))

  override def map2[A,B,C](fa: F[A], fb:F[B])(f:(A,B) => C) : F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E,x]})#f] =
    new Monad[({type f[x] = Either[E,x]})#f] {
      override def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A,B](ea: Either[E,A])(f: A => Either[E,B]) = ea match {
        case Right(a) => f(a)
        case Left(b) => Left(b)
      }
    }
}
