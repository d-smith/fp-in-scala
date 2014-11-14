package state

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers
import state.RNG.SimpleRNG


class RNGTest extends WordSpec with MustMatchers {
  "RNG produces the same psuedo random number sequence when given the same seed" in {
    var rng1:RNG = SimpleRNG(42)
    var rng2:RNG = SimpleRNG(42)

    assert(rng1.nextInt._1 === rng2.nextInt._1)

    for(i <- 1 to 100000) {
      val (res1, tempRNG1) = rng1.nextInt
      val (res2, tempRNG2) = rng2.nextInt
      assert(res1 === res2)
      rng1 = tempRNG1
      rng2 = tempRNG2
    }

  }

  "nonNegativeInt always produces non negative ints" in {
    var generator: RNG = SimpleRNG(42)
    for(i <- 1 to 100000) {
      val (generated, nextGen) = RNG.nonNegativeInt(generator)
      assert(generated >= 0)
      generator = nextGen
    }
  }
  "double produces a number between 0 and 1" in {
    var rng: RNG = SimpleRNG(22)
    for(i <- 1 to 100000) {
      val (d,r) = RNG.double(rng)
      assert(d >= 0.0)
      assert(d < 1.0)
      rng = r
    }
  }

  "int double produces int double tuples" in {
    var gen1: RNG = SimpleRNG(1)
    var gen2: RNG = SimpleRNG(1)
    val((i1,d1), _) = RNG.intDouble(gen1)
    val((i2,d2), _) = RNG.intDouble(gen2)
    assert(i1 === i2)
    assert(d1 === d2)
  }

  "double int produces int double tuples" in {
    var gen1: RNG = SimpleRNG(1)
    var gen2: RNG = SimpleRNG(1)
    val((d1,i1), _) = RNG.intDouble(gen1)
    val((d2,i2), _) = RNG.intDouble(gen2)
    assert(i1 === i2)
    assert(d1 === d2)
  }

  "int produces a list of random integers" in {
    var rng1: RNG = SimpleRNG(42)
    val rng2: RNG = SimpleRNG(42)

    var l = List[Int]()
    for(i <- 1 to 3) {
      val (i, r) = rng1.nextInt
      l = i :: l
      rng1 = r
    }

    val l2 = RNG.ints(3)(rng2)

    assert(l2._1 === l)

  }
}
