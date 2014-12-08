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

    assert(prop.run(10, rng1) === Passed)
  }
}
