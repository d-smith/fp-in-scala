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


}

object Option {
  def map2[A,B,C](a: Option[A], b: Option[B])(f:(A,B) => C) : Option[C] = {
    a.flatMap(aa => b.map(bb => f(aa,bb)))
  }

  def sequence[A](a:List[Option[A]]) : Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(head => sequence(t).map(head :: _))
  }

  def sequenceViaFold[A](a:List[Option[A]]) : Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((x,y)=> map2(x,y)(_ :: _))
  }

  def traverse[A,B](a:List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h),traverse(t)(f))(_ :: _)
  }

  def traverseViaFold[A,B](a:List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((x,y) => map2(f(x),y)(_ :: _))
  }
}



case class Some[+A](get:A) extends Option[A]
case object None extends Option[Nothing]
