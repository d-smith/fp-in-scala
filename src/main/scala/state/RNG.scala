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


}