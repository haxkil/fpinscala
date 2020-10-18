package fpinscala.gettingstarted

import org.scalatest.funspec.AnyFunSpec

class PolymorphicFunctionsTest extends AnyFunSpec {
  import PolymorphicFunctions._

  describe("PolymorphicFunctionsTest") {
    it("should isSorted") {
      assertResult(true)(isSorted(Array(), Ordering.Int.gt))
      assertResult(true)(isSorted(Array(0), Ordering.Int.gt))
      assertResult(true)(isSorted(Array(0, 2), Ordering.Int.gt))
      assertResult(true)(isSorted(Array(0, 2, 3), Ordering.Int.gt))
      assertResult(false)(isSorted(Array(0, 3, 2), Ordering.Int.gt))
      assertResult(false)(isSorted(Array(2, 3, 0), Ordering.Int.gt))
    }
  }
}
