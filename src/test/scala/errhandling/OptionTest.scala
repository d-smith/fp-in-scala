package errhandling

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers
import scala.util.Try


class OptionTest extends WordSpec with MustMatchers {
  //Hide standard Option class for testing our version
  import scala.{Option => _}

  def convertStringToInt(s: String) : Option[Int] = {
    try {
      Some(s.toInt)
    }  catch {
      case _:NumberFormatException => None
    }
  }

  "An option with some value must apply a mapping function" in {
    val s:Option[Int] = Some(1)
    assert(s.map(_+1) === Some(2))
  }

  "An option with no value returns none when a mappin function is applied" in {
    val s:Option[Int] = None
    assert(s.map(_+1) === None)
  }

  "flatMap applies it function when not None" in {
    def f(a:Int) : Option[Int] = {
      a match {
        case 2 => None
        case _ => Some(a + 1)
      }
    }

    val s:Option[Int] = Some(1)
    assert(s.flatMap(f) === Some(2))

    val a:Option[Int] = Some(2)
    assert(a.flatMap(f) === None)

    assert(None.flatMap(f) === None)
  }

  "getOrElse returns option value when it isn't none" in {
    val o:Option[Int] = Some(2)
    assert(o.getOrElse(3) === 2)
  }

  "getOrElse returns argument when option is none" in {
    assert(None.getOrElse(3) === 3)
  }

  "orElse returns the option when it is some" in {
    val a: Option[Int] = Some(42)
    assert(a.orElse(Some(2)) === Some(42))
  }

  "orElse returns its argument when invoked on None" in {
    assert(None.orElse(Some(42)) === Some(42))
  }

  "Filter returns some when the filter predicate returns true" in {
    assert(Some(42).filter(_ == 42) === Some(42))
  }

  "Filter returns None when the filter predicate returns false" in {
    assert(Some(242).filter(_ == 42) === None)
  }

  "Filter returns None when called on None" in {
    assert(None.filter(_ => true) === None)
  }

  "Sequence returns a list of A given a list of Option[A]" in {
    val a = List(Some(1), Some(2), Some(3))
    assert(Option.sequence(a) === Some(List(1,2,3)))
    assert(Option.sequenceViaFold(a) === Some(List(1,2,3)))
  }

  "Sequence returns none if any list of Option[A] elements are None" in {
    val a = List(Some(1), Some(2), None)
    assert(Option.sequence(a) === None)
    assert(Option.sequenceViaFold(a) === None)
  }

  "Traverse returns mapped list from list of options if all ops work and there are no none elements" in {
    val a = List("1","2","3")
    assert(Option.traverse(a)(convertStringToInt) === Some(List(1,2,3)))
    assert(Option.traverseViaFold(a)(convertStringToInt) === Some(List(1,2,3)))
  }

  "Traverse returns None if a conversion fails on a list element" in {
    val a = List("1", "2", "three!")
    assert(Option.traverse(a)(convertStringToInt) === None)
    assert(Option.traverseViaFold(a)(convertStringToInt) === None)
  }

}
