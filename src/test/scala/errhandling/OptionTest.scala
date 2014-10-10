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

}
