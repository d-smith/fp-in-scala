package gettingstarted

import org.scalatest.FlatSpec


class PartialAndCurryingTest extends FlatSpec {
  import gettingstarted.Currying.partial1
  import gettingstarted.Currying.curry
  import gettingstarted.Currying.compose
  import gettingstarted.Currying.uncurry

  def add(a:Int, b:Int) : Int = a + b
  def printSum(sum: Int) : String = s"sum is $sum"

  "Partially applied functions" can "be completed" in {
    val inc = partial1(1, add)
    assert(inc(2) === 3)
  }

  "Curried functions" can "be applied" in {
    val curried = curry(add)
    assert(curried(1)(2) === 3)
  }

  "Functions" can "be composed using compose" in {
    val inc = partial1(1, add)
    val composed = compose(printSum, inc)
    assert(composed(3) === "sum is 4")
  }

  "A curried function" can "be uncurried" in {
    val curried = curry(add)
    val uncurried = uncurry(curried)
    assert(uncurried(40,2) === 42)
  }

}
