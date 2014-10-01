package fds

import scala.collection.mutable.ListBuffer

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

    if(n <= 0) l
    else  l match {
        case Nil => Nil
        case Cons(h,t) => drop(t, n - 1)
      }
   }

  def dropWhile[A](l: List[A])(f: A => Boolean) : List[A] = {
    l match {
      case Nil => Nil
      case Cons(h,t) =>
        if(f(h)) dropWhile(t)(f) else l
    }
  }

  //needed a little help with init
  def reverse[A](l: List[A]) : List[A] = {
    def collectValues(acc: ListBuffer[A], l: List[A]): ListBuffer[A] = {
      l match {
        case Nil => acc
        case Cons(h,t) => acc += h; collectValues(acc, t)
      }
    }
    def listFromReverse[A](b: ListBuffer[A]) : List[A] = {
      def listFromReversedR[A](b:ListBuffer[A], l: List[A]): List[A] = {
        if(b.isEmpty) l
        else {
          val h = b.head
          listFromReversedR(b.drop(1), Cons(h,l))
        }
      }
      listFromReversedR(b,Nil)
    }
    val listVals = collectValues(new ListBuffer(), l)
    listFromReverse(listVals)
  }

  //create a list with all but the last element of the seed list
  def init[A](l:List[A]) : List[A] = {

    def initR[A](acc: List[A], l:List[A]) : List[A] = {
      l match {
        case Nil => acc
        case Cons(h, Nil) => acc
        case Cons(h,t) => initR(Cons(h,acc),t)
      }
    }
    val initBackwards = initR(Nil, l)
    reverse(initBackwards)
  }

  def foldRight[A,B](as: List[A],z:B)(f:(A,B)=>B) : B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x,foldRight(xs,z)(f))
    }
  }

  def length[A](l: List[A]) : Int = {
    foldRight(l,0)((_,acc)=> acc + 1)
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
    }
  }

}
