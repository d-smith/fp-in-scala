package fds

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*) : List[A] = if(as.isEmpty) Nil else Cons(as.head, apply(as.tail : _*))

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => t
    }
  }

  def setHead[A](h: A, l: List[A]) : List[A] = {
    l match {
      case Nil => Cons(h, Nil)
      case Cons(previous, t) => Cons(h,t)
    }
  }

  def drop[A](l:List[A], n: Int):List[A] = {
    println(s"l is $l and n is $n")

    if(n <= 0) l
    else  l match {
        case Nil => Nil
        case Cons(h,t) => drop(t, n - 1)
      }
   }

  def dropWhile[A](l: List[A], f: A => Boolean) : List[A] = {
    l match {
      case Nil => Nil
      case Cons(h,t) =>
        if(f(h)) dropWhile(t,f) else l
    }
  }

}
