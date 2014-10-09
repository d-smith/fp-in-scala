package fds

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers


class TreeTest extends WordSpec with MustMatchers {
  "A populated tree" must {
    "return its size" in {
      val t:Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
      assert(Tree.size(t) === 5)
      assert(Tree.sizeViaFold(t) === 5)
    }

    "return its max" in {
      val t:Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
      assert(Tree.maximum(t) === 3)
      assert(Tree.maximumViaFold(t) === 3)
    }
  }

  "return its max node depth" in {
    val t:Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Branch(Leaf(1), Leaf(1))))
    assert(Tree.depth(t) === 3)
    assert(Tree.depthViaFold(t) === 3)
  }

  "map leaf values via a function" in {
    val t:Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))
    val f = (a:Int) => "x" + a
    assert(Tree.map(t)(f) === Branch(Branch(Leaf("x1"), Leaf("x2")), Branch(Leaf("x3"), Branch(Leaf("x4"), Leaf("x5")))))
    assert(Tree.mapViaFold(t)(f) === Branch(Branch(Leaf("x1"), Leaf("x2")), Branch(Leaf("x3"), Branch(Leaf("x4"), Leaf("x5")))))
  }
}
