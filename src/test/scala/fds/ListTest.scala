package fds

import org.scalatest.{WordSpec}
import org.scalatest.matchers.MustMatchers

class ListTest extends WordSpec with MustMatchers {
  "A non-empty list" must {
    "produce a tail without mutating the list" in {
      val sublist: List[Int] = Cons(2, Cons(3, Nil))
      val l:List[Int] = Cons(1, sublist)

      assert(List.tail(l) === sublist)

      l match {
        case Cons(1, sublist) => ()
        case _ => fail("expected no mutation in l")
      }
    }


    "allow replacement of the head without mutation" in {
      val l: List[Int] = Cons(1, Cons(2, Cons(3,Nil)))
      val l2 = List.setHead(5,l)

      l match {
        case Cons(1, Cons(2,Cons(3,Nil))) => ()
        case _ => fail("expected no mutation in l")
      }

      l2 match {
        case Cons(5, Cons(2, Cons(3, Nil))) => ()
        case _ => fail("unexpected content or structure for l2")
      }
    }

    "allow a number of elements to be dropped" in {
      val l: List[Int] = Cons(1, Cons(2, Cons(3,Nil)))
      assert(List.drop(l, 2) === Cons(3,Nil))
    }

    "drop items while a predicate is satisfied" in {
      val f = (n: Int) => if(n > 2) true else false
      val l:List[Int] = Cons(4, Cons(3, Cons(2, Cons(1,Nil))))
      val dropped = List.dropWhile(l, f)
      assert(dropped === (Cons(2, Cons(1, Nil))))
    }
  }

  "An empty list" must {
    "produce Nil as the tail" in {
      assert(List.tail(Nil) === Nil)
    }

    "produce a new single element list when setting the head" in {
      assert(List.setHead(1,Nil) === Cons(1, Nil))
    }

    "return Nil is asked to drop elements" in {
      assert(List.drop(Nil, 10) === Nil)
    }

    "return Nil when dropWhile is called" in {
      assert(List.dropWhile(Nil, (x:Int) => true) === Nil)
    }
  }

  "A single element list" must {
    "produce Nil as the tail" in {
      assert(List.tail(Cons(1,Nil)) === Nil)
    }

    "produce a new single element list with the correct head" in {
      val l:List[Int] = Cons(1,Nil)
      val l2 = List.setHead(3,l)

      l2 match {
        case Cons(3,Nil) => ()
        case _ => fail("unexpected content or structure created via setHead")
      }
      
    }

    "produce Nil if dropping one or more elements" in {
      assert(List.drop(Cons(1,Nil), 2) === Nil)
    }

    "produce itself if dropping no elements" in {
      assert(List.drop(Cons(1,Nil),0) === Cons(1,Nil))
    }

    "produce itself is dropWhile predicate is false" in {
      val l:List[Int] = Cons(2, Nil)
      val dropped = List.dropWhile(l, (x:Int) => x > 1000)
      assert(dropped === l)
    }

    "produce Nil when dropWhile predicate is true" in {
      val l:List[Int] = Cons(0,Nil)
      assert(List.dropWhile(l, (n:Int) => n == 0) === Nil)
    }
  }
}
