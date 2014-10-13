package errhandling

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers


class OptionTest extends WordSpec with MustMatchers {
  //Hide standard Option class for testing our version
  import scala.{Option => _}

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

}
