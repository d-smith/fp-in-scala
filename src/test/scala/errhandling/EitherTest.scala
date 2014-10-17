package errhandling

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers


class EitherTest extends WordSpec with MustMatchers {
  "A right value can be mapped" in {
    val a = Right(1)
    assert(a.map(_+1) === Right(2))
  }

  "A left value remains unchanged when mapped" in {
    val e = Left(1)
    assert(e.map(_ == 1) === Left(1))
  }
}
