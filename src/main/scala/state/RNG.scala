package state


trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    //nextInt algorithm - http://en.wikipedia.org/wiki/Linear_congruential_generator
    //implementation from Functional Programming in Scala
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG) : (Int, RNG) = {
    val (generated, generator) = rng.nextInt
    (if(generated < 0) -(1 + generated) else generated, generator)
  }

  def double(rng: RNG) : (Double, RNG) = {
    val (i,r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG) : ((Int, Double), RNG) = {
    val (i, tempRng) = rng.nextInt
    val (d, nextRNG) = double(tempRng)
    ((i,d),nextRNG)
  }

  def doubleInt(rng: RNG) : ((Double, Int), RNG) = {
    val ((i,d), r) = intDouble(rng)
    ((d,i),r)
  }

  def double2(rng: RNG) : ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  //This numbers returned in the list are actually in reverse order of their generation... might be ok
  def ints(count: Int)(rng: RNG) : (List[Int], RNG) = {
    def go(n: Int, rng: RNG, l: List[Int]) : (List[Int], RNG) = {
      n match {
        case n if n > 0 =>
          val (i, r) = rng.nextInt
          go(n - 1, r, i :: l)
        case _ => (l,rng)
      }
    }

    go(count, rng, List[Int]())
  }

  type Rand[+A] = RNG => (A,RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a:A): Rand[A] =
    rng => (a,rng)

  def map[A,B](s: Rand[A])(f:A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def flatMap[A,B](f:Rand[A])(g: A => Rand[B]) : Rand[B] =
    rng => {
      val(a,r) = f(rng)
      g(a)(r)
    }

  def mapViaFlatmap[A,B](s: Rand[A])(f:A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

   val doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  val doubleViaFlatMapMap: Rand[Double] =
    mapViaFlatmap(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A],rb:Rand[B])(f: (A,B) => C): Rand[C] = {
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a,b), r2)
    }
  }

  def map2ViaFlatMap[A,B,C](ra: Rand[A],rb:Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))

  def both[A,B](ra: Rand[A], rb: Rand[B]) : Rand[(A,B)] =
    map2(ra, rb)((_,_))

  val randIntDouble: Rand[(Int, Double)] = both(int,double)

  val randDoubleInt: Rand[(Double,Int)] = both(double,int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((elem, acc) => map2(elem,acc)(_::_))

  //Sequence produces elements in order, so this will produce the same generated
  //output as the non-sequence version of ints in reverse order wrt the original ints
  def intsSeq(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

}

case class State[S, +A](run: S => (A,S))

object State {

  type Rand[A] = State[RNG,A]

  def unit[S,A](a:A):State[S,A] =
    State(s => (a,s))
}