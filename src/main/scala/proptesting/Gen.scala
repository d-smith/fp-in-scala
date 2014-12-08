package proptesting

import proptesting.Prop.{FailedCase, SuccessCount, TestCases}
import state.State
import state.RNG
import laziness.Stream

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
}

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop) : Prop = Prop {
    (n,rng) => run(n, rng) match {
      case Passed => p.run(n,rng)
      case f => f
    }
  }

  def ||(p:Prop) : Prop = Prop {
    (n,rng) => run(n, rng) match {
      case _:Falsified => p.run(n,rng)
      case p => p
    }
  }
}

case class Gen[+A](sample: State[RNG,A]) {

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]) : Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(n:Int): Gen[List[A]] = Gen.listOfN(n, this)

  def listOfN(size: Gen[Int]) : Gen[List[A]] = size.flatMap(n => listOfN(n))

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def choose(start: Int, stopExclusive: Int) : Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a : => A) : Gen[A] = Gen(State.unit(a))

  def boolean : Gen[Boolean] = Gen(State(RNG.boolean))

  def double : Gen[Double] = Gen(State(RNG.double))

  def listOfN[A](n: Int, a: Gen[A]) : Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(a.sample)))

  def forAll[A](as:Gen[A])(f: A => Boolean) : Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case(a,i) => try {
        if (f(a)) Passed else Falsified(a.toString,i)
      } catch { case e: Exception => Falsified(buildMsg(a,e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](gen: Gen[A])(rng: RNG) : Stream[A] =
    Stream.unfold(rng) { rng => Some(gen.sample.run(rng)) }

  def buildMsg[A](s:A, e: Exception) : String =
    s"test case: $s\n"+
    s"generated an exception ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def union[A](g1: Gen[A], g2: Gen[A]) : Gen[A] = boolean.flatMap(b => if(b) g1 else g2)

  def weighted[A](g1:(Gen[A], Double), g2: (Gen[A], Double)) = {
    val pG1 = g1._2 / g1._2 + g2._2
    Gen(State(RNG.double)flatMap(d => if(d >= pG1) g1._1.sample else g2._1.sample))
  }


}

case class SGen[+A](g: Int => Gen[A]) {
  def map[B](f:A => B): SGen[B] =
    SGen(g andThen (_ map f))

  def flatMap[B](f: A => Gen[B]) : SGen[B] =
    SGen(g andThen (_ flatMap f))
}
