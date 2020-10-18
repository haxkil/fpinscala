package fpinscala.gettingstarted

import org.scalatest.funspec.AnyFunSpec

class MyModuleTest extends AnyFunSpec {
  import MyModule._

  describe("MyModuleTest") {
    it("should fib") {
      assertResult(0)(fib(0))
      assertResult(1)(fib(1))
      assertResult(1)(fib(2))
      assertResult(2)(fib(3))
      assertResult(5)(fib(5))
    }
  }
}
