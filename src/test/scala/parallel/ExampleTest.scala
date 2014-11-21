package parallel

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers


class ExampleTest extends WordSpec with MustMatchers {
  "an indexed list can be summed" in {
    val ints = IndexedSeq(1,2,3,4,5,6,7,8,9,10)
    val foldLeftSum = ints.foldLeft(0)((a,b) => a + b)
    assert(foldLeftSum === Example.sum(ints))
  }
}
