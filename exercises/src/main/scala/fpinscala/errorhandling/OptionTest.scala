package fpinscala.errorhandling

import org.scalatest.funspec.AnyFunSpec

class OptionTest extends AnyFunSpec {

  describe("OptionTest") {
    it("should map") {
      val o = Some(1)
      assertResult(Some("1"))(o.map(_.toString))
    }

    it("should getOrElse") {
      val o1 = Some("str1")
      assertResult("str1")(o1.getOrElse("str2"))
      assertResult("str2")(None.getOrElse("str2"))

      assertResult("str")(Some("str").getOrElse("other"))
      assertResult("other")(None.getOrElse("other"))
    }

    it("should flatMap") {
      assertResult(Some("str1"))(Some("str").flatMap(x => Some(x + "1")))
      assertResult(None)(None.flatMap(x => Some(x)))
    }

    it("should orElse") {
      val v:AnyVal = 0
      assertResult(Some("str"))(Some("str").orElse(Some("1")))
      assertResult(Some(0))(None.orElse(Some(v)))
    }

    it("should filter") {
      assertResult(Some("str"))(Some("str").filter(_.nonEmpty))
      assertResult(None)(Some("str").filter(_.length > 3))
    }

    it("should sequence") {
      assertResult(Some(List(1, 2)))(Option.sequence(List(Some(1), Some(2))))
      assertResult(None)(Option.sequence(List(Some(1), None)))
    }

    it("should traverse") {
      val f = (a:String) => Option.Try(a.toInt)
      assertResult(Some(List(1, 2)))(Option.traverse(List("1", "2"))(f))
      assertResult(None)(Option.traverse(List("1", "int"))(f))
    }

    it("should failingFn2") {}

    it("should failingFn") {}

    it("should map2") {}

    it("should mean") {}
  }
}
