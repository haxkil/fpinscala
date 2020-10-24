package fpinscala.datastructures

import org.scalatest.funspec.AnyFunSpec

class ListTest extends AnyFunSpec {
  import List._

  describe("ListTest") {
    it("should do") {
      def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

      val v = apply("one", "two")
      println(v)
    }

    it("should sum") {
      assertResult(6)(foldLeft(List(0, 1, 2, 3), 0)(_ + _))
      assertResult(24)(foldLeft(List(1, 2, 3, 4), 1)(_ * _))
    }

    it("should init") {
      val l: List[Int] = List(1, 2, 3, 4)
      println(init(l))
    }

    it("should length") {
      assertResult(5)(length(List(1, 2, 3, 4, 5)))
    }

    it("should map") {
      val l = List(1, 2, 3)
      assertResult(List(2, 3, 4))(map(l)(_ + 1))
    }
  }
}
