package laziness

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers


class StreamTest extends WordSpec with MustMatchers {
  import Stream._
  "I can make a stream" in {
    val a = Stream(1,2,3)
    assert(a.headOption === Some(1))
  }

  "I can convert a stream to a list" in {
    val a = Stream(1,2,3,4)
    assert(a.toList === List(1,2,3,4))
  }

  "I can take the first n items from a Stream" in {
    val s = Stream(1,2,3,4)
    assert(s.take(2).toList === Stream(1,2).toList)
  }

  "take takes what's available if n is greater than content of the stream" in {
    val s = Stream(1)
    assert(s.take(2).toList === Stream(1).toList)
  }

  "take can handle an empty stream" in {
    val s = Stream()
    assert(s.take(1) === Stream())
  }

  "take returns the empty stream when n < 1" in {
    val s = Stream(1,2,3,4,5)
    assert(s.take(-1) === Stream())
  }

  "The first 3 elements are dropped" in {
    val s = Stream(1,2,3,4,5)
    assert(s.drop(3).toList === Stream(4,5).toList)
  }

  "A stream with no elements is returned when no more items remain to be dropped" in {
    val s = Stream(1)
    assert(s.drop(100) === Stream())
  }

  "Elements can be taken from a string while they satisfy a predicate" in {
    val s = Stream(1,2,3,4,5)
    assert(s.takeWhile(_<=2).toList === Stream(1,2).toList)
  }

  "An empty stream is return by takeWhile if the first element doesn't satisfy the predicate" in {
    val s = Stream(1,2,3,4,5)
    assert(s.takeWhile(_ > 2) === Stream())
  }

  "forAll returns true when all elements satisfy the predicate" in {
    val s = Stream(1,2,3,4)
    assert(s.forAll(_ > 0) === true)
  }

  "forAll returns false when any element in the stream does not satisfy the predicate" in {
    val s = Stream(1,3,2,4,5)
    assert(s.forAll(_<=3) === false)
  }

  "map a stream of ints to a stream of strings" in {
    val s = Stream(1,2,3)
    assert(s.map("" + _).toList === Stream("1","2","3").toList)
  }

  "odds are filtered out of the stream" in {
    val s = Stream(1,2,3,4,5)
    assert(s.filter(_%2 == 0).toList === Stream(2,4).toList)
  }

  "two streams are appended" in {
    assert(Stream(1,2,3).append(Stream(4,5,6)).toList === Stream(1,2,3,4,5,6).toList)
  }

  "A stream can be flat mapped" in {
    val s = Stream(1,2,3)
    assert(s.flatMap(Stream(_)).toList === Stream(1,2,3).toList)
  }

  "A finite result can be obtained from an infinite stream" in {
    val ones:Stream[Int] = Stream.constant(1)
    assert(ones.take(3).toList === Stream(1,1,1).toList)
  }

  "An infinite stream of integers can be generated" in {
    val is = Stream.from(1)
    assert(is.take(4).toList === Stream(1,2,3,4).toList)
  }

}
