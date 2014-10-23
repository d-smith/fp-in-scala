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
}
