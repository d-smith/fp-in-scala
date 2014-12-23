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
}