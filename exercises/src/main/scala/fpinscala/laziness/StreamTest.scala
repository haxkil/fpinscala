package fpinscala.laziness

import org.scalatest.funspec.AnyFunSpec

class StreamTest extends AnyFunSpec {

  it("should toList") {
    assertResult(Nil)(Stream().toList())
    assertResult(List(1, 2, 3))(Stream(1, 2, 3).toList())

    assertResult(Nil)(Stream().toListBuffered())
    assertResult(List(1, 2, 3))(Stream(1, 2, 3).toListBuffered())
  }

  it("should take") {
    assertResult(Stream())(Stream().take(3))
    assertResult(Stream())(Stream(1, 2, 3).take(0))
    assertResult(Stream(1, 2).toList())(Stream(1, 2, 3).take(2).toList())
  }

  it("should drop") {
    assertResult(Stream())(Stream().drop(3))
    assertResult(Stream(1, 2, 3).toList())(Stream(1, 2, 3).drop(0).toList())
    assertResult(Stream(3).toList())(Stream(1, 2, 3).drop(2).toList())
  }

  it("should takeWhile") {
    assertResult(Stream(1, 2).toList())(
      Stream(1, 2, 3).takeWhile(_ < 3).toList()
    )
    assertResult(Stream())(Stream(1, 2, 3).takeWhile(_ > 3))
    assertResult(Stream())(Stream[Int]().takeWhile(_ > 0))
  }

  it("should forAll") {
    assertResult(true)(Stream(1, 2, 3).forAll(_ > 0))
    assertResult(false)(Stream(1, 2, 3).forAll(_ > 3))
    assertResult(true)(Stream[Int]().forAll(_ > 3))
    assertResult(true)(Stream[Int]().forAll(_ < 3))

    assertResult(Stream(1, 2).toList())(
      Stream(1, 2, 3).takeWhile(_ < 3).toList()
    )
    assertResult(Stream())(Stream(1, 2, 3).takeWhile(_ > 3))
    assertResult(Stream())(Stream[Int]().takeWhile(_ > 0))
  }

  it("should takeWhileFold") {
    assertResult(Stream())(Stream[Int]().takeWhileFold(_ > 0))
    assertResult(Stream(1, 2).toList())(Stream(1, 2, 3).takeWhileFold(_ < 3).toList())
    assertResult(Stream(1, 2, 3).toList())(Stream(1, 2, 3).takeWhileFold(_ > 0).toList())
  }

  it("should headOption") {
    assertResult(None)(Stream().headOption)
    assertResult(1)(Stream(1, 2, 3).headOption.get)
    assertResult(1)(Stream(1).headOption.get)
  }

  it("should headOptionFold") {
    assertResult(None)(Stream().headOptionFold)
    assertResult(1)(Stream(1, 2, 3).headOptionFold.get)
    assertResult(1)(Stream(1).headOptionFold.get)
  }

  it("should map") {
    assertResult(Stream())(Stream[Int]().map(_ + 1))
    assertResult(Stream(2, 3, 4).toList())(Stream(1, 2, 3).map(_ + 1).toList())
  }

  it("should filter") {
    assertResult(Stream())(Stream[Int]().filter(_ > 1))
    assertResult(Stream(2, 3).toList())(Stream(1, 2, 3).filter(_ > 1).toList())
    assertResult(Stream(3, 2).toList())(Stream(3, 2, 1).filter(_ > 1).toList())
  }

  it("should append") {
    assertResult(Stream())(Stream[Int]().append(Stream[Int]()))
    assertResult(Stream(1, 2).toList())(Stream().append(Stream(1, 2)).toList())
    assertResult(Stream(1, 2).toList())(Stream(1, 2).append(Stream()).toList())
    assertResult(Stream(1, 2, 3, 4).toList())(Stream(1, 2).append(Stream(3, 4)).toList())
  }

  it("should flatMap") {
    assertResult(Stream())(Stream[Int]().flatMap(_ => Stream(0, 0, 0)))
    assertResult(Stream(0, 0, 0, 0, 0, 0, 0, 0, 0).toList())(Stream(1, 2, 3).flatMap(_ => Stream(0, 0, 0)).toList())
  }

  it("should constant") {
    assertResult(List(1, 1, 1))(Stream.constant(1).take(3).toList())
  }

  it("should from") {
    assertResult(List(1, 2, 3))(Stream.from(1).take(3).toList())
  }
}
