package proptesting

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers
import state.{RNG, State}
import state.RNG.SimpleRNG

class GenTest extends WordSpec with MustMatchers {
  "listOfN generates a list of the specified size" in {
    val gen = Gen(State(s => s.nextInt))

    val rng1: RNG = new SimpleRNG(1)
    val listGen = gen.listOfN(12)
    assert(listGen.sample.run(rng1)._1.size === 12)
  }

  "forAll returns passed when f returns true for all invocations" in {
    val gen = Gen(State(s => s.nextInt))
    val rng1: RNG = new SimpleRNG(1)
    val listGen = gen.listOfN(12)

    def testFn(n: List[Int]) : Boolean = {
      println(s"Invoked on $n")
      true
    }

    val prop = Gen.forAll(listGen)(testFn)

    assert(prop.run(10, 10, rng1) === Passed)
  }

  "list max is greater than or equal to every other element" in {
    import Gen._

    val rng = RNG.SimpleRNG(12)
    val smallInt = Gen.choose(-10,10)
    val maxProp = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    //Show output
    Prop.run(maxProp, rng = rng)

    assert( maxProp.run(100,100,rng) === Passed)

  }

  "list sort can be verified" in {
    import Gen._

    def listIsSorted(l: List[Int]) : Boolean = {
      l match {
        case t :: Nil => true
        case h :: t =>
          val h2 = t.head
          if(!(h <= h2)) false else listIsSorted(t)
      }
    }

    val rng = RNG.SimpleRNG(12)
    val smallInt = Gen.choose(-10,10)
    val sortProp = forAll(listOf1(smallInt)) { ns =>
      val sorted = ns.sorted
      listIsSorted(sorted)
    }

    assert(sortProp.run(100,100,rng) === Passed)

  }
 }
