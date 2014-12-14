package parser



trait Parsers[ParseError, Parser[+_]] { self =>
  def char(c: Char) : Parser[Char]
  implicit def string(s: String): Parser[String]
  def or[A](s1: Parser[A], s2: Parser[A]) : Parser[A]
  def run[A](p: Parser[A])(input:String) : Either[ParseError, A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]) : Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]) : Parser[B] = self.or(p,p2)
  }
}
