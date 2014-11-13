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

}