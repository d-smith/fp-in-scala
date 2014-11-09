package state

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers
import state.RNG.SimpleRNG


class RNGTest extends WordSpec with MustMatchers {
  "RNG produces the same psuedo random number sequence when given the same seed" in {
    var rng1:RNG = SimpleRNG(42)
    var rng2:RNG = SimpleRNG(42)

    assert(rng1.nextInt._1 === rng2.nextInt._1)

    for(i <- 1 to 10000) {
      val (res1, tempRNG1) = rng1.nextInt
      val (res2, tempRNG2) = rng2.nextInt
      assert(res1 === res2)
      rng1 = tempRNG1
      rng2 = tempRNG2
    }

  }
}
