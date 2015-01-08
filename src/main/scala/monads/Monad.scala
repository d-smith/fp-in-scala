package monads

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A) : F[A]

  def flatMap[A,B](ma: F[A])(f: A => F[B]) : F[B]

  def map[A,B](ma: F[A])(f: A => B) =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: F[A], mb : F[B])(f: (A,B) => C) : F[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))

  def sequence[A](lma:List[F[A]]) : F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma,mlb) => map2(ma,mlb)(_ :: _))

  def traverse[A,B](la: List[A])(f: A => F[B]) : F[List[B]] =
    la.foldRight(unit(List[B]()))((a,mlb) => map2(f(a),mlb)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]) : F[List[A]] =
    if(n <= 0) unit(List[A]()) else map2(ma, replicateM(n - 1, ma))(_ :: _)

  def product[A,B](ma: F[A], mb: F[B]) : F[(A,B)] = map2(ma,mb)((_,_))

  def join[A](mma: F[F[A]]) : F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def flatMapViaCompose[A,B](ma: F[A])(f: A => F[B]) : F[B] =
    compose((_:Unit) => ma,f)(())

  def filterM[A](ms: List[A])(f: A => F[Boolean]) : F[List[A]] =
    ms.foldRight(unit(List[A]()))((x,y) =>
      compose(f, (b: Boolean) => if(b) map2(unit(x),y)(_::_) else y)(x))
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

  val idMonad = new Monad[Id] {
    override def unit[A](ida: => A): Id[A] = Id(ida)
    override def flatMap[A,B](ida: Id[A])(f: A => Id[B]) : Id[B] = ida flatMap f
  }
}

case class Id[A](value: A) {
  def map[B](ida: Id[A])(f: A => B) : Id[B] = Id(f(ida.value))
  def flatMap[B](f: A => Id[B]) : Id[B] = f(value)
}


