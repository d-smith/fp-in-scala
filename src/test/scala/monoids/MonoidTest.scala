package monoids

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers

class MonoidTest extends WordSpec with MustMatchers {

  "the string monoid obeys the monoid laws" in {
    import Monoid.stringMonoid.op
    import Monoid.stringMonoid.zero
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

  "the int addition monoid obeys the monadic laws" in {
    import Monoid.intAddition.op
    import Monoid.intAddition.zero

    assert(op(op(1,2), 3) === op(1, op(2,3)))
    assert(op(10, zero) === 10)
  }

  "the int multiplication monoid obeys the monadic laws" in {
    import Monoid.intMultiplication.op
    import Monoid.intMultiplication.zero

    assert(op(op(1,2), 3) === op(1, op(2,3)))
    assert(op(10, zero) === 10)
  }

  "the boolean and monoid obeys the Monadic laws" in {
    import Monoid.booleanAnd.op
    import Monoid.booleanAnd.zero

    assert(op(op(true,true), true) === op(true, op(true,true)))
    assert(op(false, zero) === false)
    assert(op(true, zero) === true)
  }

  "the boolean or monoid obeys the Monadic laws" in {
    import Monoid.booleanOr.op
    import Monoid.booleanOr.zero

    assert(op(op(true,false), true) === op(true, op(false,true)))
    assert(op(false, zero) === false)
    assert(op(true, zero) === true)
  }


}
