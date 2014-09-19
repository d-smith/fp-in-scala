package gettingstarted

import org.scalatest.prop._
import org.scalatest.FlatSpec


class SortedTest extends FlatSpec with GeneratorDrivenPropertyChecks {
  import Sorted.isSorted

  def isOrdered(a: Int, b: Int) = if(a <= b) true else false

  def testOrderingOf = isSorted(_:Array[Int], isOrdered)

  "Generated arrays" should "be flagged as sorted when ordered" in {
    forAll { (a: Int, b: Int, c: Int, d: Int) =>
      val arrayToTest = Array(a,b,c,d)
      println(arrayToTest.mkString(","))
      if(a <= b && b <= c && c <= d) {
        assert(testOrderingOf(arrayToTest) == true)
      } else {
        assert(testOrderingOf(arrayToTest) === false)
      }
    }
  }

  "Generated arrays of the same element" should "be declared ordered" in {
    forAll { n:Int =>
      val arrayToTest = Array(n,n,n,n,n)
      assert(testOrderingOf(arrayToTest) == true)
    }
  }

  "Single element arrays" should "be ordered" in {
    forAll { n:Int =>
      assert(testOrderingOf(Array(n)) == true)
    }
  }

  "The Array[Int]()" should "test as sorted" in {
    val empty:Array[Int] = Array[Int]()
    assert(testOrderingOf(empty) == true)
  }

}
