package monoids


trait Monoid[A] {
  def op(a1: A, a2:A) : A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) : String = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A])  = a1 ++ a2
    def zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def zero: Int = 0
    def op(a1: Int, a2: Int): Int = a1 + a2
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def zero: Int = 1
    def op(a1: Int, a2: Int): Int = a1 * a2
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def zero: Boolean = false
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def zero: Boolean = true
    def op(a1: Boolean, a2: Boolean) : Boolean = a1 && a2
  }

  def optionMonoid[A]:  Monoid[Option[A]] = new Monoid[Option[A]] {
    override def zero: Option[A] = None
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def zero: (A) => A = (a:A) => a
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1.compose(a2)
  }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B) : B =
    as.foldLeft(m.zero)((b,a) => m.op(b,f(a)))

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B) : B = {
    val l = v.length
    l match {
      case 0 => m.zero
      case 1 => f(v(0))
      case _ =>
        val (p1,p2) = v.splitAt(l/2)
        m.op(foldMapV(p1,m)(f), foldMapV(p2,m)(f))
    }
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def zero = Stub("")
    def op(a: WC, b: WC) : WC = (a,b) match {
      case (Stub(s1),Stub(s2)) => Stub(s1 + s2)
      case (Stub(s), Part(l,w,r)) => Part(l + s, w, r)
      case (Part(l,w,r), Stub(s)) => Part(l,w, r + s)
      case (Part(l1,w1,r1), Part(l2,w2,r2)) => Part(l1,w1 + (if((r1 + l2).isEmpty) 0 else 1),r2)
    }
  }
}

trait Foldable[F[_]] {
  def foldRight[A,B](as:F[A])(z: B)(f: (A,B) => B) : B
  def foldLeft[A,B](as: F[A])(z: B)(f:(B,A) => B) : B
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]) : B
  def concatenate[A](as: F[A])(m: Monoid[A]) : A = foldLeft(as)(m.zero)(m.op)
  def toList[A](fa:F[A]) : List[A] = foldRight(fa)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b,a) => mb.op(b,f(a)))

  override def toList[A](as:List[A]) : List[A] = as
}

object IndexSeqFoldable extends Foldable[IndexedSeq] {
  import monoids.Monoid._
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]): B =
    foldMapV(as,mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: Stream[A])(f: (A) => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b,a) => mb.op(b,f(a)))
}

object OptionFoldable extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as match {
      case None => z
      case Some(a) => f(a,z)
    }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as match {
      case None => z
      case Some(a) => f(z, a)
    }

  override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B =
    as match {
      case None => mb.zero
      case Some(a) => f(a)
    }
}

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A],right:Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Leaf(a) => f(a,z)
      case Branch(l,r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Leaf(a) => f(z,a)
      case Branch(l,r) => foldLeft(r)(foldLeft(r)(z)(f))(f)
    }

  override def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(a) => f(a)
      case Branch(l,r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }
}