package parser

import proptesting._
import proptesting.Prop._


trait Parsers[ParseError, Parser[+_]] { self =>
  def char(c: Char) : Parser[Char] = string(c.toString).map(_.charAt(0))

  def succeed[A](a:A) : Parser[A] = string("").map(_ => a)

  implicit def string(s: String): Parser[String]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]) : ParserOps[String] = ParserOps(f(a))

  def or[A](s1: Parser[A], s2: Parser[A]) : Parser[A]
  def run[A](p: Parser[A])(input:String) : Either[ParseError, A]

  def listOfN[A](n: Int, p : Parser[A]) : Parser[List[A]]

  def map[A,B](p: Parser[A])(f: A => B) : Parser[B]

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]) : Parser[B]

  def many[A](p: Parser[A]) : Parser[List[A]]

  def slice[A](p: Parser[A]) : Parser[String]

  def product[A,B](p: Parser[A], p2: Parser[B]) : Parser[(A,B)]

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C]

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]) : Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]) : Parser[B] = self.or(p,p2)

    def map[B](f: A => B) : Parser[B] = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def slice: Parser[String] = self.slice(p)

    def many = self.many(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in:Gen[String]) : Prop =
      Gen.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]) : Prop =
      equal(p, map(p)(a => a))(in)
  }
}
