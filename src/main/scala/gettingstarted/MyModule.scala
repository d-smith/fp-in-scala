package gettingstarted

object MyModule {
  def abs(n: Int) : Int =
    if(n < 0) -n else n

  def factorial(n: Int) : Int = {
    @annotation.tailrec
    def go(n:Int, acc: Int) : Int = {
      if(n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n,1)
  }


  def formatResult(name: String, n: Int, f: Int => Int) =
    s"The $name of $n is ${f(n)}"


  def main(args: Array[String]) : Unit = {
    println(formatResult("Absolute value", -42, abs))
    println(formatResult("Factorial", 7, factorial))
  }

}
