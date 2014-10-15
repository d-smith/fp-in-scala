package errhandling


sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }
  }

  def flatMap[B](f: A => Option[B]) : Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B) : B = {
    this match {
      case None => default
      case Some(x) => x
    }
  }

  def orElse[B >: A](ob: => Option[B]) : Option[B] = map(Some(_)) getOrElse ob

  def filter(f: A => Boolean) : Option[A] = flatMap(a => if(f(a))Some(a) else None)

  def map2[A,B,C](a: Option[A], b: Option[B])(f:(A,B) => C) : Option[C] = {
    a.flatMap(aa => b.map(bb => f(aa,bb)))
  }
}



case class Some[+A](get:A) extends Option[A]
case object None extends Option[Nothing]
