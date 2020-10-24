package fpinscala.datastructures

import org.scalatest.funspec.AnyFunSpec

class TreeTest extends AnyFunSpec {
  import Fixtures._
  import fpinscala.datastructures.Tree._

  describe("TreeTest") {
    it("should size") {
      assertResult(7)(size(t1))
    }

    it("should maximum") {
      assertResult(4)(maximum(t2))
    }

    it("should depth") {
      assertResult(3)(depth(t1))
      assertResult(3)(depth(t2))
      assertResult(4)(depth(t3))
      assertResult(2)(depth(t4))
    }

    it("should map") {
      assertResult(Branch(Branch(Leaf(2), Leaf(5)), Branch(Leaf(3), Leaf(4))))(map(t2)(_ + 1))
      assertResult(Branch(Branch(Leaf("1"), Leaf("4")), Branch(Leaf("2"), Leaf("3"))))(map(t2)(_ toString))
    }
  }

  object Fixtures {
    def t1 = Branch(Branch(Leaf('a'), Leaf('b')), Branch(Leaf('c'), Leaf('d')))
    def t2 = Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(2), Leaf(3)))
    def t3 = Branch(Branch(Leaf(1), Leaf(4)), Branch(Branch(Leaf(0), Leaf(2)), Leaf(3)))
    def t4 = Branch(Leaf(), Leaf())
  }
}
