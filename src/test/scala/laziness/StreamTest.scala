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
}
