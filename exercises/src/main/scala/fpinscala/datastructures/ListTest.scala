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

    it("should sum") {}

    it("should product2") {}

    it("should length") {}

    it("should product") {}

    it("should dropWhile") {}

    it("should init") {
      val l: List[Int] = List(1, 2, 3, 4)
      println(init(l))

    }

    it("should apply") {}

    it("should foldRight") {}

    it("should drop") {}

    it("should map") {}

    it("should foldLeft") {}

    it("should x") {}

    it("should sum2") {}

    it("should tail") {}

    it("should setHead") {}

    it("should append") {}
  }
}
