package fds

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]) : Int = {
      tree match {
        case Leaf(_) => 1
        case Branch(l,r) => 1 + size(l) + size(r)
      }
  }

  def maximum(t: Tree[Int]) : Int = {
    t match {
      case Leaf(v) => v
      case Branch(l,r) => maximum(l) max maximum(r)
    }
  }

  def depth[A](t: Tree[A]) : Int = {
    t match {
      case Leaf(v) => 0
      case Branch(l,r) => (depth(l) max depth(r)) + 1
    }
  }

  def map[A,B](t: Tree[A])(f: A => B ) : Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A,B](t:Tree[A])(f:A => B)(g:(B,B) => B) : B = {
    t match {
      case Leaf(a) => f(a)
      case Branch(l,r) => g(fold(l)(f)(g),fold(r)(f)(g))
    }
  }

  def sizeViaFold[A](tree: Tree[A]) : Int = fold(tree)(_ => 1)(1 + _ + _)

  def depthViaFold[A](tree: Tree[A]) : Int = fold(tree)(_ => 0)((dl,dr) => (dl max dr) + 1)

  def maximumViaFold(tree:Tree[Int]) : Int = fold(tree)(a => a)((ml,mr) => ml max mr)

  def mapViaFold[A,B](tree: Tree[A])(f: A => B ) : Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}