package laziness


import Stream._
trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }

  def toList : List[A] = {
    def toListR(acc: List[A], s: Stream[A]) : List[A] = s match {
      case Cons(h,t) => toListR(h() :: acc, t())
      case _ => acc
    }
    toListR(List(), this).reverse
  }

  def take(n:Int) : Stream[A] = {
    if(n > 0) {
      this match {
        case Cons(h,t) if n == 1 => cons(h(), Stream.empty)
        case Cons(h,t) => cons(h(), t().take(n-1))
        case _ => Stream.empty
      }
    }
    else Stream()
  }

  def takeUnfold(n:Int) : Stream[A] =
    unfold((this,n)) {
      case (Cons(h,t),n) if n == 1 => Some((h(), (Stream.empty, n - 1)))
      case (Cons(h,t),n) if (n > 0) => Some((h(),(t(), n - 1)))
      case _ => None
    }

  def drop(n:Int) : Stream[A] = {
   if(n <= 0) this
    else this match {
     case Cons(h,t) => t().drop(n - 1)
     case _ => Stream()
   }
  }

  def takeWhile(f: A => Boolean) : Stream[A] =
    foldRight(Stream[A]())((a,b)=> if (f(a)) cons(a,b) else Stream.empty)

  def takeWhileUnfold(f: A => Boolean) : Stream[A] =
    unfold(this) {
      case Cons(h,t) if f(h()) => Some(h(),t())
      case _ => None
    }

  def forAll(p: A => Boolean) : Boolean =
    foldRight(true)((a,b)=> p(a) && b)

  def foldRight[B](z: => B)(f:(A, =>B)=>B) : B =
    this match {
      case Cons(h,t) => f(h(),t().foldRight(z)(f))
      case _ => z
    }

  def map[B](f: A => B ) : Stream[B] = {
    foldRight(Stream[B]())((h,t) => cons(f(h),t))
  }

  def mapUnfold[B](f: A => B) : Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some(f(h()),t())
      case _ => None
    }

  def filter[B](f: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((h,t) =>
      if(f(h)) cons(h,t) else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t)=> cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream[B]())((h,t) => f(h).append(t))



}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]) : Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, ()=>tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*) : Stream[A] =
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A) : Stream[A] =
    cons(a, constant(a))

  def from(n: Int) : Stream[Int] =
    cons(n, from(n + 1))

  def fibs() : Stream[Int] = {
    def fibsR(n1: Int, n2: Int) : Stream[Int] = {
      cons(n1, fibsR(n2, n1 + n2))
    }
    fibsR(0,1)
  }

  def fibsUnfold() : Stream[Int] =
    unfold((0,1)) { case(f0,f1) => Some((f0,(f1, f0 + f1))) }

  def fromUnfold(n: Int) : Stream[Int] =
    unfold(n) { case x => Some(x, x+1)}

  def constantUnfold[A](a: A) : Stream[A] =
    unfold(a) { case a => Some(a,a)}


  def unfold[A,S](z: S)(f: S => Option[(A,S)]) : Stream[A] =
    f(z) match {
      case Some((a,s)) => cons(a, unfold(s)(f))
      case None => Stream.empty
    }

  def zipWith[A,B](as: Stream[A], bs: Stream[A])(f: (A,A)=> B) : Stream[B] =
    (as,bs) match {
      case (Cons(ah, at), Cons(bh,bt)) => cons(f(ah(), bh()), zipWith(at(), bt())(f))
      case (_, empty: Stream[A]) => Stream[B]()
      case (empty, _) => Stream[B]()

    }

}