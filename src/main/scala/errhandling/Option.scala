package errhandling


sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }
  }
}



case class Some[+A](get:A) extends Option[A]
case object None extends Option[Nothing]
