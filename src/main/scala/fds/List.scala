package fds

import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

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

  def foldRight[A,B](as: List[A],z:B)(f:(A,B)=>B) : B = foldLeft(reverse(as),z)((b,a)=>f(a,b))

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
    }
  }

  def length[A](l: List[A]) : Int = foldLeft(l, 0:Int)((acc,_) => acc + 1)

  def sum(l: List[Int]) : Int = foldLeft(l,0)(_+_)
  def product(l:List[Double]) = foldLeft(l,1.0)(_*_)

  def reverse[A](l:List[A]) : List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h, acc))

  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1,a2)(Cons(_,_))

  def concat[A](l:List[List[A]]) : List[A] = {
    foldRight(l,Nil:List[A])(append)
  }

  def map[A,B](l:List[A])(f:A => B) : List[B] = {
    foldRight(l, Nil:List[B])((a,b)=>Cons(f(a),b))
  }

  def flatMap[A,B](l:List[A])(f: A => List[B]):List[B] = {
    //concat(map(l)(f))
    foldRight(l, Nil:List[B])((a,b)=>append(f(a),b))
  }

  def filter[A](l:List[A])(f:A => Boolean) =
    foldRight(l, Nil:List[A])((a,b)=> if(f(a)) Cons(a,b) else b)

  def flatmapFilter[A](l:List[A])(f:A => Boolean): List[A] = flatMap(l)(a => if(f(a)) List(a) else Nil)

  def addPairs(a: List[Int], b: List[Int]) : List[Int] =  zipWith(a,b)(_+_)


  def zipWith[A,B](a: List[A], b:List[A])(f: (A,A) => B) : List[B] = {
    (a,b) match {
      case(Cons(ah,at), Cons(bh, bt)) => Cons(f(ah,bh), zipWith(at,bt)(f))
      case _ => Nil
    }
  }

  def startsWith[A](l: List[A], as: List[A]) : Boolean = {
    (l,as) match {
      case (_, Nil) => true
      case (Cons(lh, lt), Cons(ash, ast)) => if(lh != ash) false else startsWith(lt, ast)
      case _ => false
    }
  }

  def hasSubsequence[A](l: List[A], as: List[A]) : Boolean = {
    l match {
      case Nil => false
      case Cons(h,t) if(startsWith(l,as)) => true
      case Cons(h,t) => hasSubsequence(t,as)
    }
  }

}
