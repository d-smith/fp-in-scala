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

  def listOfN(n:Int): Gen[List[A]] = Gen.listOfN(n, this)

  def listOfN(size: Gen[Int]) : Gen[List[A]] = size.flatMap(n => listOfN(n))
}

object Gen {
  def choose(start: Int, stopExclusive: Int) : Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a : => A) : Gen[A] = Gen(State.unit(a))

  def boolean : Gen[Boolean] = Gen(State(RNG.boolean))

  def double : Gen[Double] = Gen(State(RNG.double))

  def listOfN[A](n: Int, a: Gen[A]) : Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(a.sample)))

  def forAll[A](a:Gen[A])(f: A => Boolean) : Prop = ???

  def union[A](g1: Gen[A], g2: Gen[A]) : Gen[A] = boolean.flatMap(b => if(b) g1 else g2)

  def weighted[A](g1:(Gen[A], Double), g2: (Gen[A], Double)) = {
    val pG1 = g1._2 / g1._2 + g2._2
    Gen(State(RNG.double)flatMap(d => if(d >= pG1) g1._1.sample else g2._1.sample))
  }
}
