package errhandling

import scala.{Option => _, Either => _, _}

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }
}

case class Left[+E](value: E) extends Either[E,Nothing]
case class Right[+A](value: A) extends Either[Nothing,A]
