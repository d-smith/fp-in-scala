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

  def drop(n:Int) : Stream[A] = {
   if(n <= 0) this
    else this match {
     case Cons(h,t) => t().drop(n - 1)
     case _ => Stream()
   }
  }

  def takeWhile(f: A => Boolean) : Stream[A] = this match {
    case Cons(h,t) if f(h()) => cons(h(), t().takeWhile(f))
    case _ => Stream.empty
  }

  def forAll(p: A => Boolean) : Boolean = this match {
    case Cons(h,t) => p(h()) && t().forAll(p)
    case _ => true
  }

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
}