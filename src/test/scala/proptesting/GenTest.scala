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
    val maxProp = forAll(listOf(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    //Show output
    Prop.run(maxProp, rng = rng)

    //Now test - current iteration does not handle empty lists, so
    //assert will come after checkin of next exercise.
    println( maxProp.run(100,100,rng) )

  }
}
