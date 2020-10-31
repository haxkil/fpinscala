package fpinscala.laziness

import java.time.LocalDate

import org.scalatest.funspec.AnyFunSpec

class StreamTest extends AnyFunSpec {
  import Stream._

  it("shoudl") {
    val start = LocalDate.MIN.toEpochDay
    val end = LocalDate.MAX.toEpochDay

    println(LocalDate.ofEpochDay(start).getYear)
    println(LocalDate.ofEpochDay(end).getYear)
    println(LocalDate.now().getYear)
//    Gen.choose(start, end).map(LocalDate.ofEpochDay(_)).suchThat(ld => {
//      ld.get(YEAR_OF_ERA) > 2000
//      ld.getYear < 2050
//
//    })
  }

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

  describe("StreamTest") {
    it("should") {
      val x = () => {
        println("expensive x")
        22
      }

      val c = cons[Int](x())
      println("obj created")
      println(c.t())
      println(c.t())
//      println(c.t())

    }

    def cons[T](v: => T): Cons[T] = {
      lazy val w = { println("calc v"); v }
      Cons(() => { println("calc w"); w })
    }

    case class Cons[T](t: () => T)

    it("should from") {}

    it("should unfold") {}
  }

}
