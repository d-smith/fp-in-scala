package fds

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers


class TreeTest extends WordSpec with MustMatchers {
  "A populated tree" must {
    "return its size" in {
      val t:Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
      assert(Tree.size(t) === 5)
    }

    "return its max" in {
      val t:Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
      assert(Tree.maximum(t) === 3)
    }
  }
}
