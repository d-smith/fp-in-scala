package monoids

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers
import monoids.Monoid._
import scala.Some

/**
 * Tests in this class check the monoid laws - associativity of the op and the use of
 * zero in op that provides identity
 */
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

  "the option monoid obeys the Monadic laws" in {
    import Monoid.optionMonoid

    val x = Some(1)
    val y = Some(2)
    val z = Some(3)

    assert(optionMonoid[Int].op(optionMonoid[Int].op(x,y),z) === optionMonoid[Int].op(x,optionMonoid[Int].op(y,z)))
    assert(optionMonoid[Int].op(x, optionMonoid[Int].zero) === x)
  }

  "the endo monoid obeys the monadic laws" in {
    import Monoid.endoMonoid

    val x = (a: Int) => a + 1
    val y = (a: Int) => a + 2
    val z = (a: Int) => a + 3

    val em1 = endoMonoid[Int].op(endoMonoid[Int].op(x,y),z)
    val em2 = endoMonoid[Int].op(x,endoMonoid[Int].op(y,z))

    assert(em1(2) === em2(2))

    val em3 = endoMonoid[Int].op(x, (a:Int) => a)
    val em4 = endoMonoid[Int].op(x, endoMonoid[Int].zero)
    assert(em3(2) === em4(2))
  }

  "the wc monoid obeys the monoid laws" in {
    import wcMonoid._

    val s1 = Stub("foo")
    var s2 = Stub("bar")
    var s3 = Stub("baz")

    assert(op(op(s1,s2), s3) === op(s1, op(s2,s3)))
    assert(op(s1, zero) === s1)
  }

}
