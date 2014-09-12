package gettingstarted

import org.scalatest.{FlatSpec}


class SortedTest extends FlatSpec {
  import Sorted.isSorted

  def isOrdered(a: Int, b: Int) = if(a <= b) true else false

  def testOrderingOf = isSorted(_:Array[Int], isOrdered)

  "The Array(1,2,2,3,3,3)" should "test as sorted" in {
        assert(testOrderingOf(Array(1,2,2,3,3,3)) == true)
  }

  "The Array(1,1,1)" should "test as sorted" in {
    assert(testOrderingOf(Array(1,1,1)) == true)
  }

  "The Array(1)" should "test as sorted" in {
    assert(testOrderingOf(Array(1)) == true)
  }


  "The Array[Int]()" should "test as sorted" in {
    val empty:Array[Int] = Array[Int]()
    assert(testOrderingOf(empty) == true)
  }

  "The Array(1,2,3,4,5,5,4,7" should "test as not sorted" in {
    assert(testOrderingOf(Array(1,2,3,4,5,5,4,7)) == false)
  }

  "The Array(2,1)" should "test as not sorted" in {
    assert(testOrderingOf(Array(2,1)) == false)
  }


}
