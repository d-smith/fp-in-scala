package state

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers
import state.RNG.SimpleRNG


class StateTest extends WordSpec with MustMatchers {
  "unit can be used to manufacture a State instance that returns a constant value" in {
    val generator: State[RNG,Int] = State.unit(12)

    val (i1,r1) = generator.run(new SimpleRNG(1))
    assert(i1 === 12)
    val (i2,r2) = generator.run(r1)
    assert(i2 === 12)
  }
}
