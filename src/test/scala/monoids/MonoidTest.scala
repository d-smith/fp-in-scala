package monoids

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers

class MonoidTest extends WordSpec with MustMatchers {
  import Monoid.stringMonoid.op
  import Monoid.stringMonoid.zero
  "the string monoid obeys the monoid laws" in {
    assert(op(op("x","y"), "z") === op("x", op("y","z")))
    assert(op("a", zero) === "a")
  }

  "the list monoid obeys the monodic laws" in {
    import Monoid.listMonoid

    val x = List(1,2,3)
    val y = List(4,5,6)
    val z = List(1,3,5)

    assert(listMonoid[Int].op(listMonoid[Int].op(x,y),z) === listMonoid[Int].op(x,listMonoid[Int].op(y,z)))
    assert(listMonoid[Int].op(x, listMonoid[Int].zero) === x)
  }


}
