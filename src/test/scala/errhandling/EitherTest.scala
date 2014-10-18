package errhandling

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers


class EitherTest extends WordSpec with MustMatchers {

  def testFn(a: Int) : Either[Int, Int] = {
    if(a < 0) Left(a) else Right(a + 1)
  }

  "A right value can be mapped" in {
    val a = Right(1)
    assert(a.map(_+1) === Right(2))
  }

  "A left value remains unchanged when mapped" in {
    val e = Left(1)
    assert(e.map(_ == 1) === Left(1))
  }

  "A right value is mapped using flatMap" in {
    val a = Right(1)
    assert(a.flatMap(testFn) === Right(2))
  }

  "A left value is not mapped when flatMap is called" in {
    val a = Left(1)
    assert(a.flatMap(testFn) === Left(1))
  }
}
