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

case class Gen[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]) : Gen[B] = Gen(sample.flatMap(a => f(a).sample))
}

object Gen {
  def choose(start: Int, stopExclusive: Int) : Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a : => A) : Gen[A] = Gen(State.unit(a))

  def boolean : Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, a: Gen[A]) : Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(a.sample)))

  def forAll[A](a:Gen[A])(f: A => Boolean) : Prop = ???
}
