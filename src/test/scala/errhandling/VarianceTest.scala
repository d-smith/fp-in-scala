package errhandling

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers


class VarianceTest extends WordSpec with MustMatchers{
  import Variance.variance
  "the variance of a sequence of doubles returns some Double" in {
      assert(variance(Seq(1.0, 1.0, 1.0, 1.0)) === Some(0.0))
  }

  "the variance of an empty sequence returns nothing" in {
    assert(variance(Seq[Double]()) === None)
  }
}
