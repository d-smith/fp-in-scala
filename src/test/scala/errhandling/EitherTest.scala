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

  "orElse returns the else on a left" in {
    val e = Left(1)
    assert(e.orElse(Right(1)) === Right(1))
  }

  "orElse returns the right value when either is a right" in {
    val a = Right(1)
    assert(a.orElse(Right(2)) === Right(1))
  }

  "map 2 combines right values" in {
    val a = Right(1)
    val b = Right(1)
    assert(a.map2(b)(_+_) === Right(2))
  }

  "map2 returns a Left if either value to be combined is a left" in {
    assert(Right(1).map2(Left(-1))(_+_) === Left(-1))
    val left: Either[Int,Int] = Left(-1)
    assert(left.map2(Right(1))(_+_) === Left(-1))
  }

  "sequence returns list of right vals when no left is in list" in {
    val es = List(Right(1), Right(2), Right(3))
    assert(Either.sequence(es) === Right(List(1,2,3)))
  }

  "sequence returns 1st left val in list" in {
    val es = List(Right(1), Right(2), Left(3), Right(4), Left(5), Right(6))
    assert(Either.sequence(es) === Left(3))
  }

  "traverse returns mapped list when no lefts are present" in {
    val es = List(1,2,3)
    assert(Either.traverse(es)(a => Right(a + 1)) === Right(List(2,3,4)))
  }

  "traverse returns 1st left val in list" in {
    def addOneMostly(a: Int) : Either[Int,Int] = if(a == 2) Left(2) else Right(a)

    val es = List(1,2,3)
    assert(Either.traverse(es)(addOneMostly) === Left(2))
  }
}
