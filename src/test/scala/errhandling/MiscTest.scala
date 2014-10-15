package errhandling

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers


class MiscTest extends WordSpec with MustMatchers{
  import Misc.variance
  
  "the variance of a sequence of doubles returns some Double" in {
      assert(variance(Seq(1.0, 1.0, 1.0, 1.0)) === Some(0.0))
  }

  "the variance of an empty sequence returns nothing" in {
    assert(variance(Seq[Double]()) === None)
  }

  "map2 combines two options using a function that does not take option args" in {
    val plus = (a:Int, b:Int) => a + b
    assert(errhandling.None.map2(Some(1), Some(2))(plus) === Some(3))
  }

  "map 2 returns None when an input argument is None" in {
    val minus = (a:Int, b:Int) => a - b
    assert(errhandling.None.map2(None, Some(1))(minus) === None)
    assert(errhandling.None.map2(Some(2), None)(minus) === None)
    assert(errhandling.None.map2(None,None)(minus) === None)
  }
}
