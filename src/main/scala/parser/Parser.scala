package parser



trait Parsers[ParseError, Parser[+_]] { self =>
  def char(c: Char) : Parser[Char]
  implicit def string(s: String): Parser[String]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]) : ParserOps[String] = ParserOps(f(a))

  def or[A](s1: Parser[A], s2: Parser[A]) : Parser[A]
  def run[A](p: Parser[A])(input:String) : Either[ParseError, A]

  def listOfN[A](n: Int, p : Parser[A]) : Parser[List[A]] = ???

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]) : Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]) : Parser[B] = self.or(p,p2)

    def map[A,B](p: Parser[A])(f: A => Parser[B]) : Parser[B] = ???

    def many[A](p: Parser[A]) : Parser[List[A]] = ???
  }
}
