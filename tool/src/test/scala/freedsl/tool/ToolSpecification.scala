package freedsl.tool

import org.scalatest._

class ToolSpecification extends FlatSpec with Matchers {

  "until" should "call the monad until the test gets false" in {
    import cats.data._
    type OneArg[A] = State[Int, A]
    val increment: OneArg[Int] = State[Int, Int](i => (i + 1, i + 1))
    increment.until((x: Int) => x >= 10).run(0).value should equal((10, 10))
  }

}