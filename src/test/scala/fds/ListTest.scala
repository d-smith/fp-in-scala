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
  }

  "An empty list" must {
    "produce Nil as the tail" in {
      assert(List.tail(Nil) === Nil)
    }

    "produce a new single element list when setting the head" in {
      assert(List.setHead(1,Nil) === Cons(1, Nil))
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

  }
}
