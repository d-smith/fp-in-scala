package proptesting

import proptesting.Prop.{FailedCase, SuccessCount}
import state.State
import state.RNG

object Prop {
  type SuccessCount = Int
  type FailedCase = String
}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

case class Gen[A](sample: State[RNG,A])

object Gen {
  def choose(start: Int, stopExclusive: Int) : Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a : => A) : Gen[A] = Gen(State.unit(a))

  def listOfN[A](n: Int, a: Gen[A]) : Gen[List[A]] = ???

  def forAll[A](a:Gen[A])(f: A => Boolean) : Prop = ???
}
