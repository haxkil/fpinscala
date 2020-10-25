package fpinscala.errorhandling

import org.scalatest.funspec.AnyFunSpec

class EitherTest extends AnyFunSpec {

  describe("EitherTest") {
    it("should map") {
      assertResult(Right("str1"))(Right("str").map(_+"1"))
      assertResult(Left("str"))((Left("str"): Either[String, String]).map(_+"1"))
    }
  }
}
